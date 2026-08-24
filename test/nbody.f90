!*****************************************************************************************
!>
!  Use the gif module to animate a gravitational three-body simulation:
!  the famous figure-eight choreography, in which three equal masses
!  chase each other around a single stable, periodic figure-eight orbit.
!  Each body is drawn with a fading trail so its recent path is visible.
!
!# See also
!  * Chenciner, A.; Montgomery, R. (2000). "A remarkable periodic solution
!    of the three-body problem in the case of equal masses".

    program nbody

    use, intrinsic :: iso_fortran_env, only: wp=>real64
    use gif_module

    implicit none

    integer,parameter  :: n_bodies      = 3     !! number of bodies
    integer,parameter  :: width         = 400   !! image width
    integer,parameter  :: height        = 400   !! image height
    integer,parameter  :: nframes       = 150   !! number of animation frames (one full period)
    integer,parameter  :: steps_per_frame = 40  !! integration substeps between rendered frames
    integer,parameter  :: trail_len     = 50    !! number of trail points kept per body
    integer,parameter  :: nlevels       = 20    !! brightness levels per body (trail fade)
    real(wp),parameter :: gravity_g     = 1.0_wp
    real(wp),parameter :: softening     = 0.05_wp !! avoids singular forces during close approaches
    real(wp),parameter :: period        = 6.32591398_wp !! period of the figure-eight orbit

    real(wp),dimension(n_bodies)   :: mass
    real(wp),dimension(n_bodies,2) :: pos,vel
    real(wp),dimension(n_bodies,trail_len,2) :: trail  !! trail(:,1,:) = most recent position

    integer,dimension(:,:,:),allocatable :: pixel     !! pixel values
    integer,dimension(3,0:n_bodies*nlevels) :: colormap  !! index 0 = black background

    integer,dimension(3,n_bodies),parameter :: base_color = reshape( &
        [255, 70, 70, &   !! body 1: red
          70,255, 90, &   !! body 2: green
          90,150,255], &  !! body 3: blue
        [3,n_bodies])

    real(wp) :: dt,scale
    integer  :: iframe,istep,ibody,k,level,color,px,py

    !------------------------------------------------------------------
    ! build the color palette: index 0 = background, then n_bodies groups
    ! of nlevels shades (dim -> bright) for each body's trail
    !------------------------------------------------------------------
    colormap(:,0) = [0,0,0]
    do ibody=1,n_bodies
        do level=1,nlevels
            color = (ibody-1)*nlevels + level
            colormap(:,color) = nint(base_color(:,ibody)*real(level,wp)/nlevels)
        end do
    end do

    !------------------------------------------------------------------
    ! figure-eight three-body initial conditions (equal unit masses)
    !------------------------------------------------------------------
    mass = 1.0_wp
    pos(1,:) = [ 0.97000436_wp,-0.24308753_wp]
    pos(2,:) = -pos(1,:)
    pos(3,:) = [0.0_wp,0.0_wp]
    vel(3,:) = [-0.93240737_wp,-0.86473146_wp]
    vel(1,:) = -0.5_wp*vel(3,:)
    vel(2,:) = -0.5_wp*vel(3,:)

    trail = 0.0_wp
    do ibody=1,n_bodies
        do k=1,trail_len
            trail(ibody,k,:) = pos(ibody,:)
        end do
    end do

    scale = 0.35_wp*min(width,height)  !! orbit extent is roughly [-1,1], leave a margin

    dt = period/(nframes*steps_per_frame)

    allocate(pixel(nframes,width,height))

    do iframe=1,nframes

        do istep=1,steps_per_frame
            call leapfrog_step(pos,vel,mass,dt)
        end do

        !push the new positions into each body's trail buffer:
        do ibody=1,n_bodies
            do k=trail_len,2,-1
                trail(ibody,k,:) = trail(ibody,k-1,:)
            end do
            trail(ibody,1,:) = pos(ibody,:)
        end do

        !render this frame from scratch using the trail buffers:
        pixel(iframe,:,:) = 0
        do ibody=1,n_bodies
            do k=trail_len,1,-1  !! draw oldest (dim) first, newest (bright) last
                level = max(1, nint(nlevels*real(trail_len-k+1,wp)/trail_len))
                color = (ibody-1)*nlevels + level
                px = nint(width/2.0_wp  + scale*trail(ibody,k,1))
                py = nint(height/2.0_wp - scale*trail(ibody,k,2))
                call draw_disk(px,py,merge(3,1,k==1),color)
            end do
        end do

    end do

    call write_animated_gif('nbody.gif',pixel,colormap,delay=3)

    contains
!*****************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Advance the n-body system by one leapfrog (kick-drift-kick) step.

        subroutine leapfrog_step(p,v,m,h)

        implicit none

        real(wp),dimension(n_bodies,2),intent(inout) :: p,v
        real(wp),dimension(n_bodies),intent(in)       :: m
        real(wp),intent(in) :: h

        real(wp),dimension(n_bodies,2) :: acc

        call compute_acceleration(p,m,acc)
        v = v + 0.5_wp*acc*h
        p = p + v*h
        call compute_acceleration(p,m,acc)
        v = v + 0.5_wp*acc*h

        end subroutine leapfrog_step
    !*************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Compute the gravitational acceleration on each body due to all others.

        subroutine compute_acceleration(p,m,acc)

        implicit none

        real(wp),dimension(n_bodies,2),intent(in)  :: p
        real(wp),dimension(n_bodies),intent(in)    :: m
        real(wp),dimension(n_bodies,2),intent(out) :: acc

        integer :: i,j
        real(wp),dimension(2) :: dr
        real(wp) :: r2,r3

        acc = 0.0_wp
        do i=1,n_bodies
            do j=1,n_bodies
                if (i==j) cycle
                dr = p(j,:) - p(i,:)
                r2 = dr(1)**2 + dr(2)**2 + softening**2
                r3 = r2*sqrt(r2)
                acc(i,:) = acc(i,:) + gravity_g*m(j)*dr/r3
            end do
        end do

        end subroutine compute_acceleration
    !*************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Draw a small filled disk of the given radius and color, clipped to
    !  the image bounds.

        subroutine draw_disk(cx,cy,radius,disk_color)

        implicit none

        integer,intent(in) :: cx,cy,radius,disk_color

        integer :: i,j

        do i=max(1,cx-radius),min(width,cx+radius)
            do j=max(1,cy-radius),min(height,cy+radius)
                if ((i-cx)**2+(j-cy)**2<=radius**2) pixel(iframe,i,j) = disk_color
            end do
        end do

        end subroutine draw_disk
    !*************************************************************************************

    end program nbody
!*****************************************************************************************
