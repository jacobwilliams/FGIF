!*****************************************************************************************
!>
!  Use the gif module to animate A* pathfinding on a weighted grid with
!  obstacles and difficult terrain: the search frontier is shown expanding
!  outward from the start until it reaches the goal, then the resulting
!  lowest-cost path is revealed.

    program pathfinding

    use, intrinsic :: iso_fortran_env, only: wp=>real64
    use gif_module

    implicit none

    integer,parameter :: n_rows        = 30   !! grid rows
    integer,parameter :: n_cols        = 30   !! grid columns
    integer,parameter :: cell_px       = 10   !! pixels per grid cell
    integer,parameter :: capture_every = 3    !! render a frame every n cells closed/revealed
    integer,parameter :: pause_frames  = 20   !! extra frames to hold on the final solution

    integer,parameter :: cost_open   = 1   !! easy terrain (grass)
    integer,parameter :: cost_rough  = 3   !! difficult terrain (mud)
    integer,parameter :: cost_block  = -1  !! impassable obstacle

    integer,parameter :: max_frames = n_rows*n_cols*2 + pause_frames + 10

    integer,dimension(n_rows,n_cols) :: terrain   !! cost_open, cost_rough, or cost_block
    logical,dimension(n_rows,n_cols) :: closed,in_open
    real(wp),dimension(n_rows,n_cols) :: g_score,f_score
    integer,dimension(n_rows,n_cols) :: parent_r,parent_c

    integer,dimension(:,:,:),allocatable :: pixel     !! pixel values
    integer,dimension(3,0:6)             :: colormap  !! color palette (see indices below)

    integer,parameter :: c_block   = 0
    integer,parameter :: c_open    = 1
    integer,parameter :: c_rough   = 2
    integer,parameter :: c_start   = 3
    integer,parameter :: c_goal    = 4
    integer,parameter :: c_visited = 5
    integer,parameter :: c_path    = 6

    integer :: iframe,seed_size
    integer,dimension(:),allocatable :: seed
    logical :: found

    colormap(:,c_block)   = [0,0,0]       !! obstacle: black
    colormap(:,c_open)    = [140,220,140] !! easy terrain: light green
    colormap(:,c_rough)   = [180,140,90]  !! difficult terrain: brown/mud
    colormap(:,c_start)   = [0,200,0]     !! start cell: bright green
    colormap(:,c_goal)    = [220,0,0]     !! goal cell: red
    colormap(:,c_visited) = [120,170,255] !! explored by the search: light blue
    colormap(:,c_path)    = [255,165,0]   !! final lowest-cost path: orange

    call random_seed(size=seed_size)
    allocate(seed(seed_size))
    seed = 3
    call random_seed(put=seed)

    call build_terrain()

    allocate(pixel(max_frames,n_cols*cell_px,n_rows*cell_px))
    iframe = 0

    call a_star_search(found)

    if (found) call reveal_path()

    !hold on the final result:
    call render_frame(force=.true.)
    do iframe = iframe+1, min(iframe+pause_frames,max_frames)
        pixel(iframe,:,:) = pixel(iframe-1,:,:)
    end do

    call write_animated_gif('pathfinding.gif',pixel(1:iframe,:,:),colormap,delay=5)

    contains
!*****************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Randomly scatter some difficult-terrain patches and obstacles
    !  across the grid (leaving the start and goal cells clear).

        subroutine build_terrain()

        implicit none

        integer :: k,cr,cc,rad,i,j
        real(wp) :: rnd

        terrain = cost_open

        !a few rough-terrain patches:
        do k=1,6
            call random_number(rnd); cr = 1+nint(rnd*(n_rows-1))
            call random_number(rnd); cc = 1+nint(rnd*(n_cols-1))
            call random_number(rnd); rad = 3+nint(rnd*4)
            do i=max(1,cr-rad),min(n_rows,cr+rad)
                do j=max(1,cc-rad),min(n_cols,cc+rad)
                    if ((i-cr)**2+(j-cc)**2<=rad**2) terrain(i,j) = cost_rough
                end do
            end do
        end do

        !a few obstacles (impassable blocks):
        do k=1,8
            call random_number(rnd); cr = 1+nint(rnd*(n_rows-1))
            call random_number(rnd); cc = 1+nint(rnd*(n_cols-1))
            call random_number(rnd); rad = 2+nint(rnd*3)
            do i=max(1,cr-rad),min(n_rows,cr+rad)
                do j=max(1,cc-rad),min(n_cols,cc+rad)
                    if ((i-cr)**2+(j-cc)**2<=rad**2) terrain(i,j) = cost_block
                end do
            end do
        end do

        !keep the start and goal clear:
        terrain(1,1) = cost_open
        terrain(n_rows,n_cols) = cost_open

        end subroutine build_terrain
    !*************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Run A* search from (1,1) to (n_rows,n_cols), rendering a frame
    !  every `capture_every` cells closed.

        subroutine a_star_search(found)

        implicit none

        logical,intent(out) :: found

        integer,dimension(4),parameter :: dr = [-1, 1, 0, 0]
        integer,dimension(4),parameter :: dc = [ 0, 0,-1, 1]

        integer  :: r,c,nr,nc,i,step
        real(wp) :: best_f,tentative_g

        closed = .false.
        in_open = .false.
        g_score = huge(1.0_wp)
        f_score = huge(1.0_wp)
        parent_r = 0; parent_c = 0

        g_score(1,1) = 0.0_wp
        f_score(1,1) = heuristic(1,1)
        in_open(1,1) = .true.
        found = .false.
        step = 0

        do

            !find the open cell with the lowest f_score:
            best_f = huge(1.0_wp)
            r = 0; c = 0
            do i=1,n_rows
                do nc=1,n_cols
                    if (in_open(i,nc) .and. f_score(i,nc)<best_f) then
                        best_f = f_score(i,nc); r = i; c = nc
                    end if
                end do
            end do

            if (r==0) exit  !! open set is empty: no path found

            if (r==n_rows .and. c==n_cols) then
                found = .true.
                exit
            end if

            in_open(r,c) = .false.
            closed(r,c) = .true.

            do i=1,4
                nr = r+dr(i); nc = c+dc(i)
                if (nr<1 .or. nr>n_rows .or. nc<1 .or. nc>n_cols) cycle
                if (terrain(nr,nc)==cost_block .or. closed(nr,nc)) cycle

                tentative_g = g_score(r,c) + terrain(nr,nc)
                if (tentative_g<g_score(nr,nc)) then
                    parent_r(nr,nc) = r; parent_c(nr,nc) = c
                    g_score(nr,nc) = tentative_g
                    f_score(nr,nc) = tentative_g + heuristic(nr,nc)
                    in_open(nr,nc) = .true.
                end if
            end do

            step = step + 1
            if (mod(step,capture_every)==0) call render_frame()

        end do

        end subroutine a_star_search
    !*************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Admissible Manhattan-distance heuristic to the goal (n_rows,n_cols).

        real(wp) function heuristic(r,c)

        implicit none

        integer,intent(in) :: r,c

        heuristic = real(abs(n_rows-r)+abs(n_cols-c),wp)

        end function heuristic
    !*************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Reconstruct the lowest-cost path from the goal back to the start,
    !  then reveal it progressively, frame by frame.

        subroutine reveal_path()

        implicit none

        integer,dimension(n_rows*n_cols,2) :: path
        integer :: path_len,pr,pc,i

        path_len = 1
        path(path_len,:) = [n_rows,n_cols]
        pr = n_rows; pc = n_cols
        do while (.not. (pr==1 .and. pc==1))
            i = parent_r(pr,pc); pc = parent_c(pr,pc); pr = i
            path_len = path_len + 1
            path(path_len,:) = [pr,pc]
        end do

        do i=path_len,1,-1
            call render_frame(path=path(i:path_len,:))
        end do

        end subroutine reveal_path
    !*************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Fill in the next animation frame from the current search state.

        subroutine render_frame(path,force)

        implicit none

        integer,dimension(:,:),intent(in),optional :: path
        logical,intent(in),optional                :: force

        integer :: r,c,color,x0,x1,y0,y1,k
        logical,dimension(n_rows,n_cols) :: on_path

        if (iframe>=max_frames .and. .not. present(force)) return
        iframe = min(iframe+1,max_frames)

        on_path = .false.
        if (present(path)) then
            do k=1,size(path,1)
                on_path(path(k,1),path(k,2)) = .true.
            end do
        end if

        do r=1,n_rows
            do c=1,n_cols

                select case (terrain(r,c))
                case (cost_block); color = c_block
                case (cost_rough); color = c_rough
                case default;      color = c_open
                end select

                if (closed(r,c)) color = c_visited
                if (on_path(r,c)) color = c_path
                if (r==1 .and. c==1) color = c_start
                if (r==n_rows .and. c==n_cols) color = c_goal

                x0 = (c-1)*cell_px+1
                x1 = c*cell_px
                y0 = (r-1)*cell_px+1
                y1 = r*cell_px
                pixel(iframe,x0:x1,y0:y1) = color

            end do
        end do

        end subroutine render_frame
    !*************************************************************************************

    end program pathfinding
!*****************************************************************************************
