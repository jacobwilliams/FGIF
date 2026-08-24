!*****************************************************************************************
!>
!  Use the gif module to animate a rotating 3D wireframe sphere: a
!  latitude/longitude mesh is rotated in 3D, projected onto the screen
!  with a simple perspective projection, and its edges are drawn with a
!  depth-based color gradient (closer edges brighter/warmer, farther
!  edges darker/cooler). Hidden-line removal (backface culling) is used
!  so that mesh lines on the far side of the sphere are not drawn.

    program rotating_sphere

    use, intrinsic :: iso_fortran_env, only: wp=>real64
    use gif_module

    implicit none

    integer,parameter  :: width      = 300   !! image width
    integer,parameter  :: height     = 300   !! image height
    integer,parameter  :: nframes    = 90    !! number of animation frames
    integer,parameter  :: ncolors    = 256   !! number of colors in the depth gradient
    integer,parameter  :: n_lat      = 14    !! number of latitude rings (including poles)
    integer,parameter  :: n_lon      = 20    !! number of longitude segments
    real(wp),parameter :: focal      = 250.0_wp  !! focal length (perspective strength)
    real(wp),parameter :: cam_dist   = 3.5_wp    !! camera distance from the sphere's center
    real(wp),parameter :: radius     = 1.0_wp    !! sphere radius
    real(wp),parameter :: axis_len   = 1.6_wp    !! length of the axis arrows (sticking out past the sphere)

    integer,parameter  :: ndepth_colors = ncolors-4  !! reserve the last 4 indices for background + axis colors
    integer,parameter  :: c_background = ncolors-4  !! black
    integer,parameter  :: c_xaxis = ncolors-3  !! red
    integer,parameter  :: c_yaxis = ncolors-2  !! green
    integer,parameter  :: c_zaxis = ncolors-1  !! blue

    integer,dimension(:,:,:),allocatable :: pixel     !! pixel values
    integer,dimension(3,0:ncolors-1)     :: colormap  !! depth-based color gradient

    real(wp),dimension(n_lat+1) :: theta_arr  !! polar angle of each latitude ring
    real(wp),dimension(n_lon)   :: phi_arr    !! azimuthal angle of each longitude segment
    real(wp),dimension(n_lat+1,n_lon) :: zcam !! camera-space depth of each vertex

    integer  :: iframe,i,j,color
    real(wp) :: t,ax,ay,az,zmin,zmax,avg_zcam

    !build a warm(near)-to-cool(far) color gradient, then the background/axis colors:
    do i=0,ndepth_colors-1
        colormap(:,i) = [max(0,255-i/2), max(0,200-i), min(255,100+i)]
    end do
    colormap(:,c_background) = [0,0,0]  !! background: black
    colormap(:,c_xaxis) = [255,40,40]   !! x-axis: red
    colormap(:,c_yaxis) = [40,255,40]   !! y-axis: green
    colormap(:,c_zaxis) = [60,120,255]  !! z-axis: blue

    !latitude/longitude angles of the sphere mesh (fixed; the mesh only
    !rotates as a rigid body, so these parametric angles never change):
    do i=1,n_lat+1
        theta_arr(i) = (i-1)*acos(-1.0_wp)/n_lat  !! polar angle: 0 (north pole) to pi (south pole)
    end do
    do j=1,n_lon
        phi_arr(j) = (j-1)*2.0_wp*acos(-1.0_wp)/n_lon  !! azimuthal angle
    end do

    allocate(pixel(nframes,width,height))

    zmin = cam_dist - radius
    zmax = cam_dist + radius

    do iframe=1,nframes

        pixel(iframe,:,:) = c_background  !! clear to black

        t = 2.0_wp*acos(-1.0_wp)*(iframe-1)/nframes
        ax = 0.4_wp*t
        ay = 1.0_wp*t
        az = 0.0_wp

        do i=1,n_lat+1
            do j=1,n_lon
                zcam(i,j) = z_of(rotate(sphere_point(theta_arr(i),phi_arr(j)),ax,ay,az))
            end do
        end do

        !draw the meridians (longitude lines), connecting adjacent latitude rings:
        do j=1,n_lon
            do i=1,n_lat
                avg_zcam = 0.5_wp*(zcam(i,j)+zcam(i+1,j))
                color = depth_color(avg_zcam)
                call draw_mesh_edge(theta_arr(i),phi_arr(j),theta_arr(i+1),phi_arr(j),ax,ay,az,color)
            end do
        end do

        !draw the parallels (latitude lines), skipping the two poles (which
        !are single points, so their "rings" would be zero-length):
        do i=2,n_lat
            do j=1,n_lon
                avg_zcam = zcam(i,j)
                color = depth_color(avg_zcam)
                call draw_mesh_edge(theta_arr(i),phi_arr(j),theta_arr(i),phi_arr(j)+2.0_wp*acos(-1.0_wp)/n_lon, &
                                    ax,ay,az,color)
            end do
        end do

        call draw_axes(ax,ay,az)

    end do

    call write_animated_gif('rotating_sphere.gif',pixel,colormap,delay=4)

    contains
!*****************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  The un-rotated point on the sphere at polar angle theta and
    !  azimuthal angle phi.

        function sphere_point(theta,phi) result(p)

        implicit none

        real(wp),intent(in) :: theta,phi
        real(wp),dimension(3) :: p

        p(1) = radius*sin(theta)*cos(phi)
        p(2) = radius*cos(theta)
        p(3) = radius*sin(theta)*sin(phi)

        end function sphere_point
    !*************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Camera-space depth of a rotated 3D point.

        real(wp) function z_of(p)

        implicit none

        real(wp),dimension(3),intent(in) :: p

        z_of = p(3) + cam_dist

        end function z_of
    !*************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Map a camera-space depth to a color index in the depth gradient.

        integer function depth_color(z)

        implicit none

        real(wp),intent(in) :: z

        depth_color = nint((ndepth_colors-1)*(1.0_wp - (z-zmin)/(zmax-zmin)))
        depth_color = max(0,min(ndepth_colors-1,depth_color))

        end function depth_color
    !*************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Draw a sphere-mesh edge between two points given by their (theta,phi)
    !  parameters, clipping it at the silhouette if only one endpoint is on
    !  the visible (camera-facing) side. The clip point is found by
    !  bisecting along the true (theta,phi) parametrization, so it lands
    !  exactly on the sphere's surface (rather than on the straight chord
    !  between the two rotated endpoints) - this is what keeps adjacent
    !  edges meeting cleanly at the silhouette instead of leaving gaps.

        subroutine draw_mesh_edge(theta0,phi0,theta1,phi1,rx,ry,rz,edge_color)

        implicit none

        real(wp),intent(in) :: theta0,phi0,theta1,phi1,rx,ry,rz
        integer,intent(in)  :: edge_color

        integer,parameter :: n_bisect = 24

        real(wp),dimension(3) :: p0,p1,pm
        real(wp) :: s0,s1,sm,tlo,thi,tm,theta_m,phi_m
        logical  :: vis0,vis1
        integer  :: iter

        p0 = rotate(sphere_point(theta0,phi0),rx,ry,rz)
        p1 = rotate(sphere_point(theta1,phi1),rx,ry,rz)

        !a point on the sphere is camera-facing when s>0 (see the backface
        !test derivation: dot(p,camera_pos-p)>0, with camera_pos=(0,0,-cam_dist)
        !and dot(p,p)=radius**2 for any point on the sphere):
        s0 = -cam_dist*p0(3) - radius**2
        s1 = -cam_dist*p1(3) - radius**2
        vis0 = s0>0.0_wp
        vis1 = s1>0.0_wp

        if (.not. vis0 .and. .not. vis1) return

        if (vis0 .and. vis1) then
            call draw_segment(p0,p1,edge_color)
            return
        end if

        !bisect along the true parametrization to find where s crosses zero:
        tlo = 0.0_wp; thi = 1.0_wp
        do iter=1,n_bisect
            tm = 0.5_wp*(tlo+thi)
            theta_m = theta0 + tm*(theta1-theta0)
            phi_m   = phi0   + tm*(phi1-phi0)
            pm = rotate(sphere_point(theta_m,phi_m),rx,ry,rz)
            sm = -cam_dist*pm(3) - radius**2
            if ((sm>0.0_wp) .eqv. vis0) then
                tlo = tm
            else
                thi = tm
            end if
        end do
        tm = 0.5_wp*(tlo+thi)
        theta_m = theta0 + tm*(theta1-theta0)
        phi_m   = phi0   + tm*(phi1-phi0)
        pm = rotate(sphere_point(theta_m,phi_m),rx,ry,rz)

        if (vis0) then
            call draw_segment(p0,pm,edge_color)
        else
            call draw_segment(pm,p1,edge_color)
        end if

        end subroutine draw_mesh_edge
    !*************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Draw rotating x/y/z axis arrows sticking out through the sphere's
    !  surface, each with a small arrowhead at the tip.

        subroutine draw_axes(rx,ry,rz)

        implicit none

        real(wp),intent(in) :: rx,ry,rz

        real(wp),parameter :: back = 0.18_wp  !! arrowhead length (back from the tip)
        real(wp),parameter :: side = 0.07_wp  !! arrowhead width

        real(wp),dimension(3,3),parameter :: dir = reshape( &
            [1.0_wp,0.0_wp,0.0_wp, 0.0_wp,1.0_wp,0.0_wp, 0.0_wp,0.0_wp,1.0_wp],[3,3])

        real(wp),dimension(3) :: origin,tip,perp1,perp2,p1,p2,p3,p4
        integer :: k,col

        origin = rotate([0.0_wp,0.0_wp,0.0_wp],rx,ry,rz)

        do k=1,3

            select case (k)
            case (1); col = c_xaxis; perp1 = [0.0_wp,1.0_wp,0.0_wp]; perp2 = [0.0_wp,0.0_wp,1.0_wp]
            case (2); col = c_yaxis; perp1 = [1.0_wp,0.0_wp,0.0_wp]; perp2 = [0.0_wp,0.0_wp,1.0_wp]
            case (3); col = c_zaxis; perp1 = [1.0_wp,0.0_wp,0.0_wp]; perp2 = [0.0_wp,1.0_wp,0.0_wp]
            end select

            tip     = rotate(dir(:,k)*axis_len,rx,ry,rz)
            p1 = rotate(dir(:,k)*(axis_len-back)+perp1*side,rx,ry,rz)
            p2 = rotate(dir(:,k)*(axis_len-back)-perp1*side,rx,ry,rz)
            p3 = rotate(dir(:,k)*(axis_len-back)+perp2*side,rx,ry,rz)
            p4 = rotate(dir(:,k)*(axis_len-back)-perp2*side,rx,ry,rz)

            call draw_segment(origin,tip,col)
            call draw_segment(tip,p1,col)
            call draw_segment(tip,p2,col)
            call draw_segment(tip,p3,col)
            call draw_segment(tip,p4,col)

        end do

        end subroutine draw_axes
    !*************************************************************************************

    !*************************************************************************************
    !> author: Jacob Williams
    !
    !  Project two already-rotated 3D points and draw the line between them.

        subroutine draw_segment(a,b,seg_color)

        implicit none

        real(wp),dimension(3),intent(in) :: a,b
        integer,intent(in) :: seg_color

        real(wp) :: za,zb,xa,ya,xb,yb

        za = a(3)+cam_dist; zb = b(3)+cam_dist
        xa = width/2.0_wp  + focal*a(1)/za
        ya = height/2.0_wp - focal*a(2)/za
        xb = width/2.0_wp  + focal*b(1)/zb
        yb = height/2.0_wp - focal*b(2)/zb

        call draw_line(nint(xa),nint(ya),nint(xb),nint(yb),seg_color)

        end subroutine draw_segment
    !*************************************************************************************

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

    end program rotating_sphere
!*****************************************************************************************
