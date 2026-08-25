!*****************************************************************************************
!>
!  Use the gif module to animate a breakout/brick breaker game simulation:
!  a ball bounces around inside a game area, breaking bricks arranged in rows
!  at the top. The player controls a paddle at the bottom to keep the ball
!  in play. The game progresses automatically without user input, following
!  a simple AI to track the ball.

    program breakout_game

    use, intrinsic :: iso_fortran_env, only: wp=>real64
    use gif_module

    implicit none

    integer,parameter :: width       = 280  !! game area width
    integer,parameter :: height      = 360  !! game area height
    integer,parameter :: nframes     = 600  !! number of animation frames
    integer,parameter :: n_brick_rows = 4   !! number of rows of bricks
    integer,parameter :: n_brick_cols = 7   !! number of columns of bricks
    integer,parameter :: brick_width  = 35  !! pixels
    integer,parameter :: brick_height = 12  !! pixels
    integer,parameter :: brick_gap    = 2   !! pixels between bricks
    integer,parameter :: paddle_width = 40  !! pixels
    integer,parameter :: paddle_height = 8  !! pixels
    real(wp),parameter :: ball_radius = 3.0_wp
    real(wp),parameter :: paddle_y = real(height-15,wp)
    real(wp),parameter :: ball_speed = 3.2_wp

    integer,dimension(:,:,:),allocatable :: pixel     !! pixel values
    integer,dimension(3,0:6)             :: colormap  !! color palette

    logical,dimension(n_brick_rows,n_brick_cols) :: brick_active
    real(wp) :: ball_x,ball_y,ball_vx,ball_vy,paddle_x
    integer :: iframe,i,j,ibrick

    !------------------------------------------------------------------
    ! color palette: 0=black background, 1=white border, 2=blue ball,
    ! 3=green paddle, 4-6=brick colors (red, yellow, orange)
    !------------------------------------------------------------------
    colormap(:,0) = [10,10,30]      !! background: dark navy
    colormap(:,1) = [220,220,220]   !! border: light gray
    colormap(:,2) = [100,200,255]   !! ball: cyan
    colormap(:,3) = [100,255,100]   !! paddle: green
    colormap(:,4) = [255,80,80]     !! brick row 1: red
    colormap(:,5) = [255,255,80]    !! brick row 2: yellow
    colormap(:,6) = [255,160,80]    !! brick row 3-4: orange

    !------------------------------------------------------------------
    ! initialize bricks (all active at start)
    !------------------------------------------------------------------
    brick_active = .true.

    !------------------------------------------------------------------
    ! initialize ball and paddle
    !------------------------------------------------------------------
    ball_x = real(width,wp)/2.0_wp
    ball_y = paddle_y - 20.0_wp
    ball_vx = 1.6_wp
    ball_vy = -sqrt(ball_speed**2 - ball_vx**2)

    paddle_x = real(width,wp)/2.0_wp - real(paddle_width,wp)/2.0_wp

    allocate(pixel(nframes,width,height))

    do iframe=1,nframes

        call update_game()

        !render this frame:
        pixel(iframe,:,:) = 0  !! black background

        !draw border:
        pixel(iframe,1:width,1) = 1
        pixel(iframe,1:width,height) = 1
        pixel(iframe,1,1:height) = 1
        pixel(iframe,width,1:height) = 1

        !draw bricks:
        do i=1,n_brick_rows
            do j=1,n_brick_cols
                if (brick_active(i,j)) then
                    ibrick = min(i,3)+3  !! color index (4, 5, or 6)
                    call draw_rect(8 + (j-1)*(brick_width+brick_gap), 20 + (i-1)*(brick_height+brick_gap), &
                                   brick_width, brick_height, ibrick)
                end if
            end do
        end do

        !draw paddle:
        call draw_rect(nint(paddle_x), nint(paddle_y), paddle_width, paddle_height, 3)

        !draw ball:
        call draw_disk(nint(ball_x), nint(ball_y), nint(ball_radius), 2)

    end do

    call write_animated_gif('breakout_game.gif',pixel,colormap,delay=2)

    contains
!*****************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Update the game state: move ball, apply physics, detect collisions.

        subroutine update_game()

        implicit none

        integer :: ii,jj,ibrick_x,ibrick_y_local
        real(wp) :: ai_target,speed_now,hit_offset

        !classic breakout kinematics: no gravity, constant-speed reflections.
        ball_x = ball_x + ball_vx
        ball_y = ball_y + ball_vy

        !wall collisions (bounce):
        if (ball_x-ball_radius<2.0_wp) then
            ball_x = 2.0_wp + ball_radius
            ball_vx = abs(ball_vx)
        end if
        if (ball_x+ball_radius>real(width-1,wp)) then
            ball_x = real(width-1,wp) - ball_radius
            ball_vx = -abs(ball_vx)
        end if
        if (ball_y-ball_radius<2.0_wp) then
            ball_y = 2.0_wp + ball_radius
            ball_vy = abs(ball_vy)
        end if

        !top-out: reset ball if it goes off the bottom:
        if (ball_y>real(height+10,wp)) then
            ball_x = real(width,wp)/2.0_wp
            ball_y = paddle_y - 20.0_wp
            ball_vx = 1.6_wp
            ball_vy = -sqrt(ball_speed**2 - ball_vx**2)
        end if

        !AI: paddle tracks the ball's x-position:
        ai_target = ball_x - real(paddle_width,wp)/2.0_wp
        ai_target = max(2.0_wp, min(real(width-paddle_width-2,wp), ai_target))
        paddle_x = paddle_x + 0.3_wp*(ai_target - paddle_x)

        !paddle collision (bounce ball):
        if (ball_y+ball_radius>paddle_y .and. ball_y-ball_radius<paddle_y+real(paddle_height,wp) &
            .and. ball_x>paddle_x .and. ball_x<paddle_x+real(paddle_width,wp)) then
            ball_y = paddle_y - ball_radius
            !Add controlled spin based on where the ball hits the paddle,
            !then renormalize so the speed stays close to ball_speed.
            hit_offset = (ball_x - (paddle_x+real(paddle_width,wp)/2.0_wp)) / (real(paddle_width,wp)/2.0_wp)
            hit_offset = max(-1.0_wp,min(1.0_wp,hit_offset))
            ball_vx = 2.0_wp*hit_offset
            ball_vy = -sqrt(max(0.5_wp,ball_speed**2 - ball_vx**2))
        end if

        !brick collisions:
        do ii=1,n_brick_rows
            do jj=1,n_brick_cols
                if (.not. brick_active(ii,jj)) cycle
                ibrick_x = 8 + (jj-1)*(brick_width+brick_gap)
                ibrick_y_local = 20 + (ii-1)*(brick_height+brick_gap)
                if (ball_x+ball_radius>real(ibrick_x,wp) .and. ball_x-ball_radius<real(ibrick_x+brick_width,wp) &
                    .and. ball_y+ball_radius>real(ibrick_y_local,wp) .and. ball_y-ball_radius<real(ibrick_y_local+brick_height,wp)) then
                    brick_active(ii,jj) = .false.
                    if (abs(ball_y-real(ibrick_y_local,wp))<abs(ball_x-real(ibrick_x,wp)) .or. &
                        abs(ball_y-real(ibrick_y_local+brick_height,wp))<abs(ball_x-real(ibrick_x+brick_width,wp))) then
                        ball_vy = -ball_vy
                    else
                        ball_vx = -ball_vx
                    end if
                    speed_now = sqrt(ball_vx**2 + ball_vy**2)
                    if (speed_now>0.0_wp) then
                        ball_vx = ball_vx * ball_speed / speed_now
                        ball_vy = ball_vy * ball_speed / speed_now
                    end if
                    return
                end if
            end do
        end do

        end subroutine update_game
    !*************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Draw a filled rectangle at (x,y) with the given width, height, and color.

        subroutine draw_rect(x,y,w,h,color)

        implicit none

        integer,intent(in) :: x,y,w,h,color
        integer :: ii,jj

        do ii=max(1,x),min(width,x+w-1)
            do jj=max(1,y),min(height,y+h-1)
                pixel(iframe,ii,jj) = color
            end do
        end do

        end subroutine draw_rect
    !*************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Draw a filled disk at (cx,cy) with the given radius and color.

        subroutine draw_disk(cx,cy,radius,disk_color)

        implicit none

        integer,intent(in) :: cx,cy,radius,disk_color
        integer :: ii,jj

        do ii=max(1,cx-radius),min(width,cx+radius)
            do jj=max(1,cy-radius),min(height,cy+radius)
                if ((ii-cx)**2+(jj-cy)**2<=radius**2) pixel(iframe,ii,jj) = disk_color
            end do
        end do

        end subroutine draw_disk
    !*************************************************************************************

    end program breakout_game
!*****************************************************************************************
