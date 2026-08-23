!*****************************************************************************************
!>
!
!  Use the gif module to animate solving a small Traveling Salesman Problem
!  instance: random stops are connected by a tour, and the tour is
!  progressively improved using the 2-opt local search heuristic. Each
!  improving swap is captured as an animation frame, so the gif shows the
!  tour untangling itself over time.

    program tsp

    use, intrinsic :: iso_fortran_env, only: wp=>real64
    use gif_module

    implicit none

    integer,parameter :: n_stops    = 25   !! number of stops (change as desired)
    integer,parameter :: width      = 400  !! image width
    integer,parameter :: height     = 400  !! image height
    integer,parameter :: margin     = 30   !! border margin in pixels
    integer,parameter :: max_frames = 500  !! max number of animation frames
    integer,parameter :: pause      = 15   !! extra frames to hold on the final tour

    integer,dimension(:,:,:),allocatable :: pixel     !! pixel values
    integer,dimension(3,0:3)             :: colormap  !! [background,line,stop,start stop]

    real(wp),dimension(n_stops)     :: xcoord,ycoord  !! stop coordinates [0,1]
    integer,dimension(n_stops)      :: px,py          !! stop coordinates [pixels]
    real(wp),dimension(n_stops,n_stops) :: dist        !! distance between stops
    integer,dimension(n_stops)      :: tour           !! order in which stops are visited

    integer  :: i,j,k,a,b,c,d,tmp,nframes,seed_size
    real(wp) :: old_len,new_len
    logical  :: improved
    integer,dimension(:),allocatable :: seed

    colormap(:,0) = [255,255,255]  !! background: white
    colormap(:,1) = [0,0,255]      !! tour lines: blue
    colormap(:,2) = [0,0,0]        !! stops: black
    colormap(:,3) = [255,0,0]      !! starting stop: red

    !use a fixed seed so the animation is reproducible:
    call random_seed(size=seed_size)
    allocate(seed(seed_size))
    seed = 42
    call random_seed(put=seed)

    call random_number(xcoord)
    call random_number(ycoord)

    do i=1,n_stops
        px(i) = margin + int(xcoord(i)*(width-2*margin))
        py(i) = margin + int(ycoord(i)*(height-2*margin))
    end do

    do i=1,n_stops
        do j=1,n_stops
            dist(i,j) = sqrt(real(px(i)-px(j),wp)**2 + real(py(i)-py(j),wp)**2)
        end do
    end do

    !initial tour: visit the stops in the (random) order they were generated:
    tour = [(i, i=1,n_stops)]

    allocate(pixel(max_frames,width,height))
    nframes = 1
    call render_frame(nframes,tour)

    !2-opt local search: repeatedly reverse a tour segment if doing so
    !shortens the total tour length, until no such improvement exists.
    do
        improved = .false.
        outer: do i=1,n_stops-1
            do j=i+2,n_stops

                if (i==1 .and. j==n_stops) cycle  !these edges are already adjacent

                a = tour(i)
                b = tour(i+1)
                c = tour(j)
                d = tour(merge(1,j+1,j==n_stops))

                old_len = dist(a,b) + dist(c,d)
                new_len = dist(a,c) + dist(b,d)

                if (new_len < old_len - 1.0e-9_wp) then
                    tour(i+1:j) = tour(j:i+1:-1)
                    improved = .true.
                    if (nframes<max_frames) then
                        nframes = nframes + 1
                        call render_frame(nframes,tour)
                    end if
                    cycle outer
                end if

            end do
        end do outer
        if (.not. improved) exit
    end do

    !hold on the final, optimized tour for a few extra frames:
    do k=1,pause
        if (nframes>=max_frames) exit
        nframes = nframes + 1
        call render_frame(nframes,tour)
    end do

    call write_animated_gif('tsp.gif',pixel(1:nframes,:,:),colormap,delay=15)

    contains
!*****************************************************************************************

    !*************************************************************************************
    !>
    !
    !  Draw the tour (stops and connecting lines) into the given frame.

        subroutine render_frame(iframe,cur_tour)

        implicit none

        integer,intent(in)                    :: iframe
        integer,dimension(n_stops),intent(in) :: cur_tour

        integer :: i1,i2,m

        pixel(iframe,:,:) = 0  !white background

        do m=1,n_stops
            i1 = cur_tour(m)
            i2 = cur_tour(merge(1,m+1,m==n_stops))
            call draw_line(iframe,px(i1),py(i1),px(i2),py(i2),1)
        end do

        do m=1,n_stops
            call draw_point(iframe,px(m),py(m),merge(3,2,m==cur_tour(1)))
        end do

        end subroutine render_frame
    !*************************************************************************************

    !*************************************************************************************
    !>
    !
    !  Draw a line from (x0,y0) to (x1,y1) using Bresenham's algorithm.

        subroutine draw_line(iframe,x0,y0,x1,y1,icolor)

        implicit none

        integer,intent(in) :: iframe,x0,y0,x1,y1,icolor

        integer :: x,y,dx,dy,sx,sy,err,e2

        x = x0; y = y0
        dx = abs(x1-x0); sx = merge(1,-1,x0<x1)
        dy = -abs(y1-y0); sy = merge(1,-1,y0<y1)
        err = dx + dy

        do
            if (x>=1 .and. x<=width .and. y>=1 .and. y<=height) pixel(iframe,x,y) = icolor
            if (x==x1 .and. y==y1) exit
            e2 = 2*err
            if (e2>=dy) then
                err = err + dy
                x = x + sx
            end if
            if (e2<=dx) then
                err = err + dx
                y = y + sy
            end if
        end do

        end subroutine draw_line
    !*************************************************************************************

    !*************************************************************************************
    !>
    !
    !  Draw a filled square of the given radius centered at (x,y).

        subroutine draw_point(iframe,x,y,icolor)

        implicit none

        integer,intent(in) :: iframe,x,y,icolor

        integer,parameter :: r = 4
        integer :: i1,j1

        do i1=max(1,x-r),min(width,x+r)
            do j1=max(1,y-r),min(height,y+r)
                pixel(iframe,i1,j1) = icolor
            end do
        end do

        end subroutine draw_point
    !*************************************************************************************

    end program tsp
!*****************************************************************************************
