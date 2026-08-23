!*****************************************************************************************
!>
!
!  Use the gif module to animate Conway's Game of Life, seeded with a
!  Gosper glider gun so that gliders stream diagonally across the board.

    program game_of_life

    use gif_module

    implicit none

    integer,parameter :: cell_size = 6    !! pixels per cell
    integer,parameter :: nrows     = 60   !! grid rows
    integer,parameter :: ncols     = 80   !! grid cols
    integer,parameter :: nframes   = 120  !! number of animation frames

    integer,dimension(:,:),allocatable   :: grid,next_grid  !! 1=alive, 0=dead
    integer,dimension(:,:,:),allocatable :: pixel           !! pixel values
    integer,dimension(3,0:1)             :: colormap        !! [black,green]

    integer :: iframe,i,j,r,c,ir,ic,alive,px,py

    colormap(:,0) = [0,0,0]     !! dead cell: black
    colormap(:,1) = [0,255,0]   !! live cell: green

    allocate(grid(nrows,ncols),next_grid(nrows,ncols))
    grid = 0

    call add_glider_gun(6,6)

    allocate(pixel(nframes,ncols*cell_size,nrows*cell_size))

    do iframe=1,nframes

        !render the current grid state to pixels:
        do r=1,nrows
            do c=1,ncols
                px = (c-1)*cell_size
                py = (r-1)*cell_size
                pixel(iframe,px+1:px+cell_size,py+1:py+cell_size) = grid(r,c)
            end do
        end do

        !advance to the next generation:
        do r=1,nrows
            do c=1,ncols
                alive = 0
                do i=-1,1
                    do j=-1,1
                        if (i==0 .and. j==0) cycle
                        ir = modulo(r-1+i,nrows)+1  !! wrap around (toroidal grid)
                        ic = modulo(c-1+j,ncols)+1
                        alive = alive + grid(ir,ic)
                    end do
                end do
                if (grid(r,c)==1) then
                    next_grid(r,c) = merge(1,0, alive==2 .or. alive==3)
                else
                    next_grid(r,c) = merge(1,0, alive==3)
                end if
            end do
        end do
        grid = next_grid

    end do

    call write_animated_gif('game_of_life.gif',pixel,colormap,delay=8)

    contains
!*****************************************************************************************

    !*************************************************************************************
    !>
    !
    !  Place a Gosper glider gun with its top-left corner at (row,col).

        subroutine add_glider_gun(row,col)

        implicit none

        integer,intent(in) :: row,col

        integer,parameter :: n = 36
        integer,dimension(2,n) :: offsets  !! (row,col) offsets of live cells

        offsets(:, 1) = [5, 1];  offsets(:, 2) = [5, 2]
        offsets(:, 3) = [6, 1];  offsets(:, 4) = [6, 2]
        offsets(:, 5) = [3, 13]; offsets(:, 6) = [3, 14]
        offsets(:, 7) = [4, 12]; offsets(:, 8) = [4, 16]
        offsets(:, 9) = [5, 11]; offsets(:,10) = [5, 17]
        offsets(:,11) = [6, 11]; offsets(:,12) = [6, 15]
        offsets(:,13) = [6, 17]; offsets(:,14) = [6, 18]
        offsets(:,15) = [7, 11]; offsets(:,16) = [7, 17]
        offsets(:,17) = [8, 12]; offsets(:,18) = [8, 16]
        offsets(:,19) = [9, 13]; offsets(:,20) = [9, 14]
        offsets(:,21) = [1, 25]; offsets(:,22) = [2, 23]
        offsets(:,23) = [2, 25]; offsets(:,24) = [3, 21]
        offsets(:,25) = [3, 22]; offsets(:,26) = [4, 21]
        offsets(:,27) = [4, 22]; offsets(:,28) = [5, 21]
        offsets(:,29) = [5, 22]; offsets(:,30) = [6, 23]
        offsets(:,31) = [6, 25]; offsets(:,32) = [7, 25]
        offsets(:,33) = [3, 35]; offsets(:,34) = [3, 36]
        offsets(:,35) = [4, 35]; offsets(:,36) = [4, 36]

        do i=1,n
            grid(row+offsets(1,i),col+offsets(2,i)) = 1
        end do

        end subroutine add_glider_gun
    !*************************************************************************************

    end program game_of_life
!*****************************************************************************************
