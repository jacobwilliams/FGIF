!*****************************************************************************************
!>
!  Use the gif module to animate a fractal plant growing, using an
!  L-system (Lindenmayer system) to generate the branching structure and
!  turtle graphics to render it. Branches near the trunk are colored
!  brown, fading to green further out along the branches.

    program lsystem_plant

    use, intrinsic :: iso_fortran_env, only: wp=>real64
    use gif_module

    implicit none

    integer,parameter  :: width         = 320   !! image width
    integer,parameter  :: height        = 320   !! image height
    integer,parameter  :: n_iterations  = 5     !! number of L-system rewrite iterations
    integer,parameter  :: capture_every = 12    !! render a frame every n drawn segments
    integer,parameter  :: pause_frames  = 25    !! extra frames to hold on the final plant
    integer,parameter  :: max_len       = 8000  !! max length of the L-system string (rule_x has 4 X's per X, so this grows ~4x per iteration)
    integer,parameter  :: max_stack     = 50    !! max turtle push/pop nesting depth
    integer,parameter  :: max_depth     = 12    !! number of colors in the brown->green gradient
    real(wp),parameter :: turn_angle    = 25.0_wp*acos(-1.0_wp)/180.0_wp  !! turtle turn angle

    !axiom and production rules for a classic Lindenmayer fractal plant:
    character(len=*),parameter :: axiom  = 'X'
    character(len=*),parameter :: rule_x = 'F+[[X]-X]-F[-FX]+X'
    character(len=*),parameter :: rule_f = 'FF'

    character(len=max_len) :: cur,nxt
    integer :: cur_len,nxt_len,iter,i,nsegments

    integer,dimension(:,:,:),allocatable :: pixel     !! pixel values
    integer,dimension(:,:),allocatable   :: canvas    !! accumulated drawing (persists across frames)
    integer,dimension(3,0:max_depth)     :: colormap  !! index 0 = black background, 1..max_depth = brown (trunk) -> green (tips) gradient

    real(wp) :: xmin,xmax,ymin,ymax,scale,xoffset

    integer :: iframe,max_frames

    !------------------------------------------------------------------
    ! 1) expand the L-system string
    !------------------------------------------------------------------
    cur = axiom
    cur_len = len(axiom)
    do iter=1,n_iterations
        nxt_len = 0
        do i=1,cur_len
            select case (cur(i:i))
            case ('X'); call append(nxt,nxt_len,rule_x)
            case ('F'); call append(nxt,nxt_len,rule_f)
            case default; call append(nxt,nxt_len,cur(i:i))
            end select
        end do
        cur(1:nxt_len) = nxt(1:nxt_len)
        cur_len = nxt_len
    end do

    nsegments = count([(cur(i:i)=='F', i=1,cur_len)])

    !------------------------------------------------------------------
    ! 2) dry run: walk the turtle (without drawing) to find the plant's
    !    bounding box, so it can be scaled to fit the image
    !------------------------------------------------------------------
    call walk_turtle(cur,cur_len,draw=.false.)

    scale   = 0.9_wp*min(width/(xmax-xmin), height/(ymax-ymin))
    xoffset = width/2.0_wp - scale*0.5_wp*(xmin+xmax)

    !------------------------------------------------------------------
    ! 3) build the color gradient (index 0 = background, 1..max_depth = trunk->tips)
    !------------------------------------------------------------------
    colormap(:,0) = [0,0,0]  !! background: black
    do i=1,max_depth
        colormap(:,i) = nint( [101.0_wp,67.0_wp,33.0_wp] + &
                              ([34.0_wp,139.0_wp,34.0_wp]-[101.0_wp,67.0_wp,33.0_wp]) &
                              *real(i-1,wp)/(max_depth-1) )
    end do

    !------------------------------------------------------------------
    ! 4) render pass: walk the turtle again, this time drawing, and
    !    periodically snapshot the accumulated canvas as an animation frame
    !------------------------------------------------------------------
    max_frames = nsegments/capture_every + pause_frames + 10
    allocate(pixel(max_frames,width,height))
    allocate(canvas(width,height))
    canvas = 0
    iframe = 0

    call walk_turtle(cur,cur_len,draw=.true.)

    !hold on the final, fully-grown plant:
    do iframe = iframe+1, min(iframe+pause_frames,max_frames)
        pixel(iframe,:,:) = canvas
    end do

    call write_animated_gif('lsystem_plant.gif',pixel(1:iframe,:,:),colormap,delay=5)

    contains
!*****************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Append a substring to a growing buffer string.

        subroutine append(buf,buf_len,addition)

        implicit none

        character(len=*),intent(inout) :: buf
        integer,intent(inout)          :: buf_len
        character(len=*),intent(in)    :: addition

        if (buf_len+len(addition)>len(buf)) &
            error stop 'lsystem_plant: max_len exceeded, increase max_len or reduce n_iterations'

        buf(buf_len+1:buf_len+len(addition)) = addition
        buf_len = buf_len + len(addition)

        end subroutine append
    !*************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Interpret the L-system string as turtle-graphics commands. If
    !  `draw` is false, this just tracks the bounding box of the turtle's
    !  path (module variables xmin,xmax,ymin,ymax). If `draw` is true, it
    !  actually draws the segments into `canvas`, snapshotting `pixel`
    !  every `capture_every` segments.

        subroutine walk_turtle(str,str_len,draw)

        implicit none

        character(len=*),intent(in) :: str
        integer,intent(in)          :: str_len
        logical,intent(in)          :: draw

        real(wp) :: x,y,ang,nx,ny
        real(wp),dimension(max_stack) :: stack_x,stack_y,stack_ang
        integer  :: depth,k,seg_count,color

        x = 0.0_wp; y = 0.0_wp; ang = acos(-1.0_wp)/2.0_wp  !! start pointing straight up
        depth = 0
        seg_count = 0

        if (.not. draw) then
            xmin = x; xmax = x; ymin = y; ymax = y
        end if

        do k=1,str_len
            select case (str(k:k))
            case ('F')
                nx = x + cos(ang)
                ny = y + sin(ang)
                if (draw) then
                    color = 1 + min(depth,max_depth-1)
                    call draw_line(nint(scale*x+xoffset), nint(height-1-scale*(y-ymin)), &
                                    nint(scale*nx+xoffset),nint(height-1-scale*(ny-ymin)),color)
                    seg_count = seg_count + 1
                    if (mod(seg_count,capture_every)==0) then
                        iframe = min(iframe+1,max_frames)
                        pixel(iframe,:,:) = canvas
                    end if
                else
                    xmin = min(xmin,nx); xmax = max(xmax,nx)
                    ymin = min(ymin,ny); ymax = max(ymax,ny)
                end if
                x = nx; y = ny
            case ('+')
                ang = ang + turn_angle
            case ('-')
                ang = ang - turn_angle
            case ('[')
                depth = depth + 1
                if (depth<=max_stack) then
                    stack_x(depth) = x; stack_y(depth) = y; stack_ang(depth) = ang
                end if
            case (']')
                if (depth<=max_stack .and. depth>=1) then
                    x = stack_x(depth); y = stack_y(depth); ang = stack_ang(depth)
                end if
                depth = depth - 1
            end select
        end do

        end subroutine walk_turtle
    !*************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Draw a line from (x0,y0) to (x1,y1) into `canvas` using Bresenham's
    !  algorithm, clipped to the image bounds.

        subroutine draw_line(x0,y0,x1,y1,line_color)

        implicit none

        integer,intent(in) :: x0,y0,x1,y1,line_color

        integer :: x,y,dx,dy,sx,sy,err,e2

        x = x0; y = y0
        dx = abs(x1-x0); sx = merge(1,-1,x0<x1)
        dy = -abs(y1-y0); sy = merge(1,-1,y0<y1)
        err = dx + dy

        do
            if (x>=1 .and. x<=width .and. y>=1 .and. y<=height) &
                canvas(x,y) = line_color
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

    end program lsystem_plant
!*****************************************************************************************
