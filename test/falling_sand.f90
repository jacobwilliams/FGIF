!*****************************************************************************************
!>
!  Use the gif module to animate a falling-sand cellular automaton:
!  sand grains are poured in from the top and cascade downward, sliding
!  diagonally when blocked, piling up into a mound. Grains are colored
!  in bands according to when they were poured, so the layered structure
!  of the pile is visible.

    program falling_sand

    use, intrinsic :: iso_fortran_env, only: wp=>real64
    use gif_module

    implicit none

    integer,parameter :: n_rows      = 100  !! grid rows
    integer,parameter :: n_cols      = 80   !! grid columns
    integer,parameter :: cell_px     = 4    !! pixels per grid cell
    integer,parameter :: nframes     = 300  !! number of animation frames
    integer,parameter :: pour_frames = 220  !! stop pouring new sand after this many frames
    integer,parameter :: grains_per_frame = 3  !! grains added per frame while pouring
    integer,parameter :: n_shades    = 8    !! number of sand color bands

    integer,dimension(:,:),allocatable   :: grid   !! 0 = empty, else a sand color index
    integer,dimension(:,:,:),allocatable :: pixel  !! pixel values
    integer,dimension(3,0:n_shades)      :: colormap  !! index 0 = background

    integer  :: iframe,i,c,color,x0,x1,y0,y1
    real(wp) :: rnd

    colormap(:,0) = [20,20,30]  !! background: near-black

    !sand color bands, from pale sandy yellow to dark burnt orange:
    do i=1,n_shades
        colormap(:,i) = nint( [255.0_wp,235.0_wp,150.0_wp] + &
                              ([160.0_wp,80.0_wp,20.0_wp]-[255.0_wp,235.0_wp,150.0_wp]) &
                              *real(i-1,wp)/(n_shades-1) )
    end do

    allocate(grid(n_rows,n_cols))
    grid = 0

    allocate(pixel(nframes,n_cols*cell_px,n_rows*cell_px))

    do iframe=1,nframes

        !pour new grains of sand in near the top-center of the grid:
        if (iframe<=pour_frames) then
            color = 1 + mod((iframe-1)/20,n_shades)
            do i=1,grains_per_frame
                call random_number(rnd)
                c = n_cols/2 + nint((rnd-0.5_wp)*10)
                c = max(1,min(n_cols,c))
                if (grid(1,c)==0) grid(1,c) = color
            end do
        end if

        call update_sand()

        !render the grid to pixels:
        do i=1,n_rows
            do c=1,n_cols
                x0 = (c-1)*cell_px+1
                x1 = c*cell_px
                y0 = (i-1)*cell_px+1
                y1 = i*cell_px
                pixel(iframe,x0:x1,y0:y1) = grid(i,c)
            end do
        end do

    end do

    call write_animated_gif('falling_sand.gif',pixel,colormap,delay=3)

    contains
!*****************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Advance the sand simulation by one step: each grain falls straight
    !  down if possible, otherwise slides diagonally (in a random left/right
    !  preference order) if one of those cells is open.

        subroutine update_sand()

        implicit none

        integer  :: r,cc,g
        real(wp) :: rr

        !sweep from the bottom row upward, so each grain moves at most
        !one row per frame (giving a smooth, gradual falling animation):
        do r=n_rows-1,1,-1
            do cc=1,n_cols

                if (grid(r,cc)==0) cycle
                g = grid(r,cc)

                if (grid(r+1,cc)==0) then
                    grid(r+1,cc) = g
                    grid(r,cc) = 0
                    cycle
                end if

                call random_number(rr)
                if (rr<0.5_wp) then
                    if (cc>1 .and. grid(r+1,cc-1)==0) then
                        grid(r+1,cc-1) = g; grid(r,cc) = 0
                    else if (cc<n_cols .and. grid(r+1,cc+1)==0) then
                        grid(r+1,cc+1) = g; grid(r,cc) = 0
                    end if
                else
                    if (cc<n_cols .and. grid(r+1,cc+1)==0) then
                        grid(r+1,cc+1) = g; grid(r,cc) = 0
                    else if (cc>1 .and. grid(r+1,cc-1)==0) then
                        grid(r+1,cc-1) = g; grid(r,cc) = 0
                    end if
                end if

            end do
        end do

        end subroutine update_sand
    !*************************************************************************************

    end program falling_sand
!*****************************************************************************************
