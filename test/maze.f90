!*****************************************************************************************
!>
!  Use the gif module to animate generating a maze (via a randomized
!  depth-first-search / recursive-backtracker algorithm) and then solving
!  it (via breadth-first search) to find the shortest path from the
!  top-left cell to the bottom-right cell.

    program maze

    use, intrinsic :: iso_fortran_env, only: wp=>real64
    use gif_module

    implicit none

    integer,parameter :: n_rows        = 15   !! maze size: number of cell rows
    integer,parameter :: n_cols        = 15   !! maze size: number of cell columns
    integer,parameter :: cell_px       = 10   !! pixels per maze cell/wall unit
    integer,parameter :: capture_every = 2    !! only render every n-th step (keeps gif small)
    integer,parameter :: pause_frames  = 20   !! extra frames to hold on the final solution

    !grid dimensions in "wall units": walls are at even indices, cells at odd indices
    integer,parameter :: gw = 2*n_cols+1
    integer,parameter :: gh = 2*n_rows+1

    integer,parameter :: max_frames = n_rows*n_cols*3 + pause_frames + 10

    logical,dimension(gh,gw)             :: open_cell  !! .true. if this grid position is a passage
    integer,dimension(:,:,:),allocatable :: pixel      !! pixel values
    integer,dimension(3,0:6)             :: colormap   !! color palette (see indices below)

    integer,parameter :: c_wall     = 0
    integer,parameter :: c_passage  = 1
    integer,parameter :: c_start    = 2
    integer,parameter :: c_goal     = 3
    integer,parameter :: c_active   = 4
    integer,parameter :: c_visited  = 5
    integer,parameter :: c_path     = 6

    logical,dimension(n_rows,n_cols) :: visited
    integer,dimension(n_rows,n_cols) :: parent_r,parent_c  !! bfs parent pointers
    integer :: iframe

    colormap(:,c_wall)    = [0,0,0]       !! wall: black
    colormap(:,c_passage) = [255,255,255] !! open passage: white
    colormap(:,c_start)   = [0,180,0]     !! start cell: green
    colormap(:,c_goal)    = [200,0,0]     !! goal cell: red
    colormap(:,c_active)  = [0,200,200]   !! actively carving cell: cyan
    colormap(:,c_visited) = [150,150,255] !! cell visited while solving: light blue
    colormap(:,c_path)    = [255,165,0]   !! final shortest path: orange

    allocate(pixel(max_frames,gw*cell_px,gh*cell_px))
    open_cell = .false.
    iframe = 0

    call generate_maze()
    call solve_maze()

    !hold on the final, fully-solved maze:
    call render_frame(force=.true.)
    do iframe = iframe+1, min(iframe+pause_frames,max_frames)
        pixel(iframe,:,:) = pixel(iframe-1,:,:)
    end do

    call write_animated_gif('maze.gif',pixel(1:iframe,:,:),colormap,delay=6)

    contains
!*****************************************************************************************

    !*************************************************************************************
    !>
    !
    !  Generate a perfect maze using an iterative randomized depth-first
    !  search (recursive backtracker), carving passages as it goes.

        subroutine generate_maze()

        implicit none

        integer,dimension(4),parameter :: dr = [-1, 1, 0, 0]
        integer,dimension(4),parameter :: dc = [ 0, 0,-1, 1]

        integer,dimension(n_rows*n_cols,2) :: stack
        integer :: nstack,r,c,i,nr,nc,nunvisited,pick,step
        integer,dimension(4) :: order

        visited = .false.
        nstack  = 1
        stack(1,:) = [1,1]
        visited(1,1) = .true.
        open_cell(1,1) = .true.
        step = 0

        do while (nstack>0)

            r = stack(nstack,1)
            c = stack(nstack,2)

            !find unvisited neighbors in random order:
            call random_order(order)
            nunvisited = 0
            pick = 0
            do i=1,4
                nr = r + dr(order(i))
                nc = c + dc(order(i))
                if (nr>=1 .and. nr<=n_rows .and. nc>=1 .and. nc<=n_cols) then
                    if (.not. visited(nr,nc)) then
                        pick = order(i)
                        exit
                    end if
                end if
            end do

            if (pick>0) then
                nr = r + dr(pick)
                nc = c + dc(pick)
                !carve the wall between (r,c) and (nr,nc):
                open_cell(2*r-1+dr(pick), 2*c-1+dc(pick)) = .true.
                open_cell(2*nr-1, 2*nc-1) = .true.
                visited(nr,nc) = .true.
                nstack = nstack + 1
                stack(nstack,:) = [nr,nc]
            else
                nstack = nstack - 1
            end if

            step = step + 1
            if (mod(step,capture_every)==0) call render_frame(active_r=r,active_c=c)

        end do

        end subroutine generate_maze
    !*************************************************************************************

    !*************************************************************************************
    !>
    !
    !  Solve the maze from the top-left cell to the bottom-right cell
    !  using breadth-first search, then reveal the shortest path.

        subroutine solve_maze()

        implicit none

        integer,dimension(4),parameter :: dr = [-1, 1, 0, 0]
        integer,dimension(4),parameter :: dc = [ 0, 0,-1, 1]

        integer,dimension(n_rows*n_cols,2) :: queue
        integer :: head,tail,r,c,i,nr,nc,step
        integer,dimension(:,:),allocatable :: path
        integer :: path_len,pr,pc,tmp_r,tmp_c

        visited = .false.
        parent_r = 0
        parent_c = 0
        head = 1
        tail = 1
        queue(1,:) = [1,1]
        visited(1,1) = .true.
        step = 0

        do while (head<=tail)

            r = queue(head,1)
            c = queue(head,2)
            head = head + 1

            if (r==n_rows .and. c==n_cols) exit

            do i=1,4
                nr = r + dr(i)
                nc = c + dc(i)
                if (nr>=1 .and. nr<=n_rows .and. nc>=1 .and. nc<=n_cols) then
                    if (.not. visited(nr,nc) .and. open_cell(2*r-1+dr(i), 2*c-1+dc(i))) then
                        visited(nr,nc) = .true.
                        parent_r(nr,nc) = r
                        parent_c(nr,nc) = c
                        tail = tail + 1
                        queue(tail,:) = [nr,nc]
                    end if
                end if
            end do

            step = step + 1
            if (mod(step,capture_every)==0) call render_frame(solver_visited=visited)

        end do

        !reconstruct the shortest path from goal back to start:
        allocate(path(n_rows*n_cols,2))
        path_len = 1
        path(path_len,:) = [n_rows,n_cols]
        pr = n_rows
        pc = n_cols
        do while (.not. (pr==1 .and. pc==1))
            tmp_r = parent_r(pr,pc)
            tmp_c = parent_c(pr,pc)
            pr = tmp_r
            pc = tmp_c
            path_len = path_len + 1
            path(path_len,:) = [pr,pc]
        end do

        !reveal the path progressively, from start to goal:
        do i=path_len,1,-1
            call render_frame(solver_visited=visited, path=path(i:path_len,:))
        end do

        end subroutine solve_maze
    !*************************************************************************************

    !*************************************************************************************
    !>
    !
    !  Fill in the next animation frame from the current maze state.

        subroutine render_frame(active_r,active_c,solver_visited,path,force)

        implicit none

        integer,intent(in),optional               :: active_r,active_c
        logical,dimension(n_rows,n_cols),intent(in),optional :: solver_visited
        integer,dimension(:,:),intent(in),optional :: path
        logical,intent(in),optional                :: force

        integer :: gi,gj,r,c,color,x0,x1,y0,y1,k
        logical,dimension(n_rows,n_cols) :: on_path

        if (iframe>=max_frames .and. .not. present(force)) return

        iframe = min(iframe+1,max_frames)

        on_path = .false.
        if (present(path)) then
            do k=1,size(path,1)
                on_path(path(k,1),path(k,2)) = .true.
            end do
        end if

        do gi=1,gh
            do gj=1,gw
                if (open_cell(gi,gj)) then
                    color = c_passage
                else
                    color = c_wall
                end if

                !color solver progress/path only at cell positions (odd,odd):
                if (mod(gi,2)==1 .and. mod(gj,2)==1) then
                    r = (gi+1)/2
                    c = (gj+1)/2
                    if (present(solver_visited)) then
                        if (solver_visited(r,c)) color = c_visited
                    end if
                    if (on_path(r,c)) color = c_path
                    if (r==1 .and. c==1) color = c_start
                    if (r==n_rows .and. c==n_cols) color = c_goal
                    if (present(active_r)) then
                        if (r==active_r .and. c==active_c) color = c_active
                    end if
                end if

                x0 = (gj-1)*cell_px+1
                x1 = gj*cell_px
                y0 = (gi-1)*cell_px+1
                y1 = gi*cell_px
                pixel(iframe,x0:x1,y0:y1) = color

            end do
        end do

        end subroutine render_frame
    !*************************************************************************************

    !*************************************************************************************
    !>
    !
    !  Return a random permutation of [1,2,3,4].

        subroutine random_order(order)

        implicit none

        integer,dimension(4),intent(out) :: order

        integer :: i,j,tmp
        real(wp) :: rnd

        order = [1,2,3,4]
        do i=4,2,-1
            call random_number(rnd)
            j = 1 + int(rnd*i)
            tmp = order(i)
            order(i) = order(j)
            order(j) = tmp
        end do

        end subroutine random_order
    !*************************************************************************************

    end program maze
!*****************************************************************************************
