!*****************************************************************************************
!>
!  Use the gif module to animate a Craig Reynolds-style "boids" flocking
!  simulation: each boid steers according to three simple local rules -
!  separation (avoid crowding neighbors), alignment (match neighbors'
!  heading), and cohesion (move toward the local flock center) - and the
!  combination produces emergent, organic flocking behavior.

    program boids

    use, intrinsic :: iso_fortran_env, only: wp=>real64
    use gif_module

    implicit none

    integer,parameter  :: n_boids           = 60    !! number of boids
    integer,parameter  :: width              = 320   !! image width
    integer,parameter  :: height             = 320   !! image height
    integer,parameter  :: nframes            = 200   !! number of animation frames
    real(wp),parameter :: perception_radius  = 40.0_wp  !! neighbors within this radius influence alignment/cohesion
    real(wp),parameter :: separation_radius  = 14.0_wp  !! neighbors within this radius trigger separation
    real(wp),parameter :: max_speed          = 3.0_wp
    real(wp),parameter :: min_speed          = 1.2_wp
    real(wp),parameter :: w_align            = 0.05_wp
    real(wp),parameter :: w_cohesion         = 0.01_wp
    real(wp),parameter :: w_separation       = 0.15_wp
    real(wp),parameter :: wall_margin        = 30.0_wp  !! start turning away from walls within this distance
    real(wp),parameter :: w_wall             = 0.3_wp

    real(wp),dimension(n_boids,2) :: pos,vel

    integer,dimension(:,:,:),allocatable :: pixel     !! pixel values
    integer,dimension(3,0:1)             :: colormap  !! [background,boid]

    integer :: iframe,i,seed_size
    integer,dimension(:),allocatable :: seed

    colormap(:,0) = [10,10,30]     !! background: dark navy
    colormap(:,1) = [180,230,255]  !! boids: pale cyan

    !use a fixed seed so the animation is reproducible:
    call random_seed(size=seed_size)
    allocate(seed(seed_size))
    seed = 7
    call random_seed(put=seed)

    call random_number(pos)
    pos(:,1) = pos(:,1)*width
    pos(:,2) = pos(:,2)*height

    call random_number(vel)
    vel = (vel-0.5_wp)*2.0_wp*max_speed

    allocate(pixel(nframes,width,height))

    do iframe=1,nframes

        call update_boids()

        pixel(iframe,:,:) = 0
        do i=1,n_boids
            call draw_boid(pos(i,:),vel(i,:))
        end do

    end do

    call write_animated_gif('boids.gif',pixel,colormap,delay=3)

    contains
!*****************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Apply one step of the separation/alignment/cohesion flocking rules
    !  (plus a gentle wall-avoidance steer) to all boids.

        subroutine update_boids()

        implicit none

        integer  :: i,j,n_near,n_sep
        real(wp) :: d
        real(wp),dimension(2) :: sum_vel,sum_pos,sum_sep,steer,acc,d_vec
        real(wp),dimension(n_boids,2) :: new_vel

        do i=1,n_boids

            sum_vel = 0.0_wp; sum_pos = 0.0_wp; sum_sep = 0.0_wp
            n_near = 0; n_sep = 0

            do j=1,n_boids
                if (i==j) cycle
                d_vec = pos(i,:) - pos(j,:)
                d = sqrt(d_vec(1)**2+d_vec(2)**2)
                if (d<perception_radius) then
                    sum_vel = sum_vel + vel(j,:)
                    sum_pos = sum_pos + pos(j,:)
                    n_near = n_near + 1
                end if
                if (d<separation_radius .and. d>1.0e-6_wp) then
                    sum_sep = sum_sep + d_vec/d
                    n_sep = n_sep + 1
                end if
            end do

            acc = 0.0_wp

            if (n_near>0) then
                acc = acc + w_align*(sum_vel/n_near - vel(i,:))
                acc = acc + w_cohesion*(sum_pos/n_near - pos(i,:))
            end if
            if (n_sep>0) acc = acc + w_separation*(sum_sep/n_sep)

            !gently steer away from the walls, rather than wrapping/bouncing:
            steer = 0.0_wp
            if (pos(i,1)<wall_margin)          steer(1) = steer(1) + (wall_margin-pos(i,1))
            if (pos(i,1)>width-wall_margin)     steer(1) = steer(1) - (pos(i,1)-(width-wall_margin))
            if (pos(i,2)<wall_margin)          steer(2) = steer(2) + (wall_margin-pos(i,2))
            if (pos(i,2)>height-wall_margin)    steer(2) = steer(2) - (pos(i,2)-(height-wall_margin))
            acc = acc + w_wall*steer

            new_vel(i,:) = vel(i,:) + acc

            !clamp speed to [min_speed,max_speed]:
            d = sqrt(new_vel(i,1)**2+new_vel(i,2)**2)
            if (d>1.0e-6_wp) then
                d = max(min_speed,min(max_speed,d))/sqrt(new_vel(i,1)**2+new_vel(i,2)**2)
                new_vel(i,:) = new_vel(i,:)*d
            end if

        end do

        vel = new_vel
        pos = pos + vel

        !clip positions to stay safely on the canvas:
        pos(:,1) = max(1.0_wp,min(real(width,wp),pos(:,1)))
        pos(:,2) = max(1.0_wp,min(real(height,wp),pos(:,2)))

        end subroutine update_boids
    !*************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Draw a boid as a small body with a "nose" line pointing in its
    !  direction of travel.

        subroutine draw_boid(p,v)

        implicit none

        real(wp),dimension(2),intent(in) :: p,v

        integer :: cx,cy,i,j,nx,ny
        real(wp) :: speed,ux,uy

        cx = nint(p(1)); cy = nint(p(2))

        do i=max(1,cx-2),min(width,cx+2)
            do j=max(1,cy-2),min(height,cy+2)
                if ((i-cx)**2+(j-cy)**2<=4) pixel(iframe,i,j) = 1
            end do
        end do

        speed = sqrt(v(1)**2+v(2)**2)
        if (speed>1.0e-6_wp) then
            ux = v(1)/speed; uy = v(2)/speed
            do i=1,6
                nx = nint(p(1)+ux*i); ny = nint(p(2)+uy*i)
                if (nx>=1 .and. nx<=width .and. ny>=1 .and. ny<=height) pixel(iframe,nx,ny) = 1
            end do
        end if

        end subroutine draw_boid
    !*************************************************************************************

    end program boids
!*****************************************************************************************
