!*****************************************************************************************
!>
!  Use the gif module to animate a rotating 3D wireframe cube: vertices are
!  rotated in 3D, projected onto the screen with a simple perspective
!  projection, and the edges are drawn with a depth-based color gradient
!  (closer edges appear brighter/warmer, farther edges darker/cooler).

    program rotating_cube

    use, intrinsic :: iso_fortran_env, only: wp=>real64
    use gif_module

    implicit none

    integer,parameter  :: width      = 300   !! image width
    integer,parameter  :: height     = 300   !! image height
    integer,parameter  :: nframes    = 90    !! number of animation frames
    integer,parameter  :: ncolors    = 256    !! number of colors in the depth gradient
    real(wp),parameter :: focal      = 250.0_wp  !! focal length (perspective strength)
    real(wp),parameter :: cam_dist   = 4.0_wp    !! camera distance from the cube's center
    real(wp),parameter :: half_size  = 1.0_wp    !! cube half-edge-length

    integer,dimension(:,:,:),allocatable :: pixel     !! pixel values
    integer,dimension(3,0:ncolors-1)     :: colormap  !! depth-based color gradient

    real(wp),dimension(3,8) :: vertex   !! the 8 cube corners
    real(wp),dimension(3,8) :: rotated  !! rotated corners
    real(wp),dimension(2,8) :: screen   !! projected 2D screen coordinates
    real(wp),dimension(8)   :: zcam     !! camera-space depth of each vertex

    integer,dimension(2,12),parameter :: edge = reshape( &
        [1,2, 2,3, 3,4, 4,1, &   !! bottom face
         5,6, 6,7, 7,8, 8,5, &   !! top face
         1,5, 2,6, 3,7, 4,8], &  !! vertical edges
        [2,12])

    integer  :: iframe,i,v1,v2,color
    real(wp) :: t,ax,ay,az,avg_zcam,zmin,zmax

    !build a warm(near)-to-cool(far) color gradient:
    do i=0,ncolors-1
        colormap(:,i) = [max(0,255-i/2), max(0,200-i), min(255,100+i)]
    end do

    !cube corners, centered at the origin:
    vertex(:,1) = [-half_size,-half_size,-half_size]
    vertex(:,2) = [ half_size,-half_size,-half_size]
    vertex(:,3) = [ half_size, half_size,-half_size]
    vertex(:,4) = [-half_size, half_size,-half_size]
    vertex(:,5) = [-half_size,-half_size, half_size]
    vertex(:,6) = [ half_size,-half_size, half_size]
    vertex(:,7) = [ half_size, half_size, half_size]
    vertex(:,8) = [-half_size, half_size, half_size]

    allocate(pixel(nframes,width,height))

    zmin = cam_dist - sqrt(3.0_wp)*half_size
    zmax = cam_dist + sqrt(3.0_wp)*half_size

    do iframe=1,nframes

        pixel(iframe,:,:) = 0  !! clear to black

        t = 2.0_wp*acos(-1.0_wp)*(iframe-1)/nframes
        ax = 0.7_wp*t
        ay = 1.0_wp*t
        az = 0.3_wp*t

        do i=1,8
            rotated(:,i) = rotate(vertex(:,i),ax,ay,az)
            zcam(i) = rotated(3,i) + cam_dist
            screen(1,i) = width/2.0_wp  + focal*rotated(1,i)/zcam(i)
            screen(2,i) = height/2.0_wp - focal*rotated(2,i)/zcam(i)
        end do

        do i=1,12
            v1 = edge(1,i)
            v2 = edge(2,i)
            avg_zcam = 0.5_wp*(zcam(v1)+zcam(v2))
            color = nint((ncolors-1)*(1.0_wp - (avg_zcam-zmin)/(zmax-zmin)))
            color = max(0,min(ncolors-1,color))
            call draw_line(nint(screen(1,v1)),nint(screen(2,v1)), &
                            nint(screen(1,v2)),nint(screen(2,v2)),color)
        end do

    end do

    call write_animated_gif('rotating_cube.gif',pixel,colormap,delay=4)

    contains
!*****************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Rotate a 3D point about the x, y, and z axes (in that order).

        function rotate(p,rx,ry,rz) result(r)

        implicit none

        real(wp),dimension(3),intent(in) :: p
        real(wp),intent(in) :: rx,ry,rz
        real(wp),dimension(3) :: r

        real(wp) :: x,y,z

        !rotate about x-axis:
        x = p(1)
        y = p(2)*cos(rx) - p(3)*sin(rx)
        z = p(2)*sin(rx) + p(3)*cos(rx)

        !rotate about y-axis:
        r(1) = x*cos(ry) + z*sin(ry)
        r(2) = y
        r(3) = -x*sin(ry) + z*cos(ry)
        x = r(1); y = r(2); z = r(3)

        !rotate about z-axis:
        r(1) = x*cos(rz) - y*sin(rz)
        r(2) = x*sin(rz) + y*cos(rz)
        r(3) = z

        end function rotate
    !*************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Draw a line from (x0,y0) to (x1,y1) using Bresenham's algorithm,
    !  clipped to the image bounds.

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
                pixel(iframe,x,y) = line_color
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

    end program rotating_cube
!*****************************************************************************************
