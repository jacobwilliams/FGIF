!*****************************************************************************************
!>
!
!  Use the gif module to create a classic "plasma" animation:
!  a smoothly-shifting, colorful interference pattern generated
!  by summing sine waves in space and time.

    program plasma

    use, intrinsic :: iso_fortran_env, only: wp=>real64
    use gif_module

    implicit none

    integer,parameter :: width   = 300  !! image width
    integer,parameter :: height  = 300  !! image height
    integer,parameter :: nframes = 60   !! number of animation frames
    integer,parameter :: ncolors = 256  !! number of colors in the palette

    integer,dimension(:,:,:),allocatable :: pixel     !! pixel values
    integer,dimension(3,0:ncolors-1)     :: colormap  !! rainbow color palette

    real(wp),parameter :: twopi = 2.0_wp*acos(-1.0_wp)

    integer  :: iframe,ix,iy,icolor
    real(wp) :: x,y,t,v

    call build_rainbow_colormap(colormap)

    allocate(pixel(nframes,width,height))

    do iframe=1,nframes

        t = twopi*(iframe-1)/nframes

        do ix=1,width
            x = real(ix,wp)/width
            do iy=1,height
                y = real(iy,wp)/height

                !classic plasma: sum of several sine waves in x, y, and time
                v = sin(10.0_wp*x + t)                       + &
                    sin(10.0_wp*y + t)                       + &
                    sin(10.0_wp*(x+y) + t)                   + &
                    sin(sqrt(200.0_wp*((x-0.5_wp)**2 + &
                                        (y-0.5_wp)**2)) + t)

                !normalize v (range -4 to 4) into a color index:
                icolor = mod(int((v+4.0_wp)/8.0_wp*(ncolors-1)),ncolors)

                pixel(iframe,ix,height-iy+1) = icolor

            end do
        end do

    end do

    call write_animated_gif('plasma.gif',pixel,colormap,delay=5)

    contains
!*****************************************************************************************

    !*************************************************************************************
    !>
    !
    !  Build a smooth rainbow colormap by cycling through the hue wheel.

        subroutine build_rainbow_colormap(colormap)

        implicit none

        integer,dimension(3,0:ncolors-1),intent(out) :: colormap

        integer :: i
        real(wp) :: h,r,g,b

        do i=0,ncolors-1
            h = real(i,wp)/ncolors  !! hue in [0,1)
            call hsv_to_rgb(h,1.0_wp,1.0_wp,r,g,b)
            colormap(:,i) = [int(255*r), int(255*g), int(255*b)]
        end do

        end subroutine build_rainbow_colormap
    !*************************************************************************************

    !*************************************************************************************
    !>
    !
    !  Convert an HSV color (h,s,v all in [0,1]) to RGB (r,g,b all in [0,1]).

        subroutine hsv_to_rgb(h,s,v,r,g,b)

        implicit none

        real(wp),intent(in)  :: h,s,v
        real(wp),intent(out) :: r,g,b

        integer  :: i
        real(wp) :: f,p,q,tt,hh

        if (s<=0.0_wp) then
            r = v; g = v; b = v
            return
        end if

        hh = h*6.0_wp
        if (hh>=6.0_wp) hh = 0.0_wp
        i = int(hh)
        f = hh - i
        p = v*(1.0_wp-s)
        q = v*(1.0_wp-s*f)
        tt = v*(1.0_wp-s*(1.0_wp-f))

        select case (i)
        case(0); r=v;  g=tt; b=p
        case(1); r=q;  g=v;  b=p
        case(2); r=p;  g=v;  b=tt
        case(3); r=p;  g=q;  b=v
        case(4); r=tt; g=p;  b=v
        case default; r=v;  g=p;  b=q
        end select

        end subroutine hsv_to_rgb
    !*************************************************************************************

    end program plasma
!*****************************************************************************************
