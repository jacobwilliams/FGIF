!*****************************************************************************************
!>
!  Use the gif module to animate a bouncing-balls simulation: spheres
!  bounce elastically inside a box, colliding with the walls and with
!  each other. Each ball is drawn in a distinct color with a fading trail
!  so recent motion is visible.

    program bouncing_balls

    use, intrinsic :: iso_fortran_env, only: wp=>real64
    use gif_module

    implicit none

    integer,parameter  :: n_balls        = 5    !! number of balls
    integer,parameter  :: width          = 320  !! image width
    integer,parameter  :: height         = 320  !! image height
    integer,parameter  :: nframes        = 300  !! number of animation frames
    integer,parameter  :: trail_len      = 40   !! number of trail points per ball
    integer,parameter  :: nlevels        = 20   !! brightness levels per ball (trail fade)
    real(wp),parameter :: radius_ball    = 5.0_wp   !! ball radius in pixels
    real(wp),parameter :: gravity        = 0.3_wp   !! downward acceleration
    real(wp),parameter :: elasticity     = 0.95_wp  !! bounce elasticity (energy loss)
    real(wp),parameter :: friction       = 0.98_wp  !! friction with walls/balls

    real(wp),dimension(n_balls)   :: posx,posy,velx,vely
    real(wp),dimension(n_balls,trail_len,2) :: trail  !! trail(:,1,:) = most recent position

    integer,dimension(:,:,:),allocatable :: pixel     !! pixel values
    integer,dimension(3,0:n_balls*nlevels) :: colormap  !! index 0 = background

    integer,dimension(3,n_balls),parameter :: base_color = reshape( &
        [255, 100, 100, &   !! ball 1: red
         100, 255, 100, &   !! ball 2: green
         100, 100, 255, &   !! ball 3: blue
         255, 255, 100, &   !! ball 4: yellow
         255, 100, 255], &  !! ball 5: magenta
        [3,n_balls])

    integer  :: iframe,iball,k,level,color
    integer,dimension(:),allocatable :: seed
    integer  :: seed_size
    real(wp) :: rnd

    !------------------------------------------------------------------
    ! build the color palette: index 0 = background, then n_balls groups
    ! of nlevels shades (dim -> bright) for each ball's trail
    !------------------------------------------------------------------
    colormap(:,0) = [0,0,0]
    do iball=1,n_balls
        do level=1,nlevels
            color = (iball-1)*nlevels + level
            colormap(:,color) = nint(base_color(:,iball)*real(level,wp)/nlevels)
        end do
    end do

    !------------------------------------------------------------------
    ! initialize balls with random positions and velocities
    !------------------------------------------------------------------
    call random_seed(size=seed_size)
    allocate(seed(seed_size))
    seed = 42
    call random_seed(put=seed)

    do iball=1,n_balls
        call random_number(rnd); posx(iball) = 20.0_wp + rnd*(width-40.0_wp)
        call random_number(rnd); posy(iball) = 20.0_wp + rnd*(height-40.0_wp)
        call random_number(rnd); velx(iball) = (rnd-0.5_wp)*8.0_wp
        call random_number(rnd); vely(iball) = (rnd-0.5_wp)*8.0_wp
    end do

    !initialize trails:
    do iball=1,n_balls
        do k=1,trail_len
            trail(iball,k,:) = [posx(iball),posy(iball)]
        end do
    end do

    allocate(pixel(nframes,width,height))

    do iframe=1,nframes

        call update_balls()

        !push the new positions into each ball's trail buffer:
        do iball=1,n_balls
            do k=trail_len,2,-1
                trail(iball,k,:) = trail(iball,k-1,:)
            end do
            trail(iball,1,:) = [posx(iball),posy(iball)]
        end do

        !render this frame from scratch using the trail buffers:
        pixel(iframe,:,:) = 0
        do iball=1,n_balls
            do k=trail_len,1,-1  !! draw oldest (dim) first, newest (bright) last
                level = max(1, nint(nlevels*real(trail_len-k+1,wp)/trail_len))
                color = (iball-1)*nlevels + level
                call draw_disk(nint(trail(iball,k,1)),nint(trail(iball,k,2)), &
                               merge(3,1,k==1),color)
            end do
        end do

    end do

    call write_animated_gif('bouncing_balls.gif',pixel,colormap,delay=2)

    contains
!*****************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Advance the ball simulation by one frame: apply gravity, update
    !  positions, resolve wall collisions, and resolve ball-ball collisions.

        subroutine update_balls()

        implicit none

        integer :: i,j
        real(wp) :: dx,dy,d,dvx,dvy,mx,my,overlap

        !apply gravity and update positions:
        do i=1,n_balls
            vely(i) = vely(i) + gravity
            posx(i) = posx(i) + velx(i)
            posy(i) = posy(i) + vely(i)
        end do

        !resolve wall collisions (with energy loss):
        do i=1,n_balls
            if (posx(i)-radius_ball<1.0_wp) then
                posx(i) = radius_ball + 1.0_wp
                velx(i) = -velx(i)*elasticity*friction
            end if
            if (posx(i)+radius_ball>real(width,wp)) then
                posx(i) = real(width,wp) - radius_ball - 1.0_wp
                velx(i) = -velx(i)*elasticity*friction
            end if
            if (posy(i)-radius_ball<1.0_wp) then
                posy(i) = radius_ball + 1.0_wp
                vely(i) = -vely(i)*elasticity*friction
            end if
            if (posy(i)+radius_ball>real(height,wp)) then
                posy(i) = real(height,wp) - radius_ball - 1.0_wp
                vely(i) = -vely(i)*elasticity*friction
            end if
        end do

        !resolve ball-ball collisions (simplified: impulse-based elastic):
        do i=1,n_balls-1
            do j=i+1,n_balls
                dx = posx(j) - posx(i)
                dy = posy(j) - posy(i)
                d = sqrt(dx**2 + dy**2)
                if (d<2.0_wp*radius_ball .and. d>1.0e-6_wp) then
                    !normalize collision direction:
                    dx = dx/d; dy = dy/d
                    !relative velocity:
                    dvx = velx(j) - velx(i)
                    dvy = vely(j) - vely(i)
                    !relative velocity along collision normal (positive = separating):
                    overlap = dvx*dx + dvy*dy
                    if (overlap<0.0_wp) then
                        !impulse along the collision normal:
                        mx = -overlap*dx*0.5_wp
                        my = -overlap*dy*0.5_wp
                        velx(i) = velx(i) - mx*elasticity
                        vely(i) = vely(i) - my*elasticity
                        velx(j) = velx(j) + mx*elasticity
                        vely(j) = vely(j) + my*elasticity
                        !separate overlapping balls:
                        d = (2.0_wp*radius_ball - d)*0.5_wp
                        posx(i) = posx(i) - d*dx
                        posy(i) = posy(i) - d*dy
                        posx(j) = posx(j) + d*dx
                        posy(j) = posy(j) + d*dy
                    end if
                end if
            end do
        end do

        end subroutine update_balls
    !*************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Draw a filled disk of the given radius and color, clipped to
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

    end program bouncing_balls
!*****************************************************************************************
