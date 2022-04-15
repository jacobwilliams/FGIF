!*****************************************************************************************
!> author: Jacob Williams
!  date: 7/31/2014
!
!  Use the gif module to create Mandelbrot set images.
!
!# Using OpenMP
!  * Compile with `-fopenmp`
!  * Set number of threads from bash: `export OMP_NUM_THREADS=4`

    program mandelbrot_example

    use, intrinsic :: iso_fortran_env, only: wp=>real64
    use gif_module

    implicit none

    integer :: i,ix,iy,n
    integer,dimension(:,:,:),allocatable :: pixel    !! pixel values
    integer,dimension(:,:),allocatable  :: colormap
    real(wp) :: xmin,xmax,ymin,ymax,xstep,ystep,x,y,tmp
    integer :: width, height, iframe, maxiter
    complex(wp) :: z, c
    real(wp) :: tstart,tend

    real(wp),external :: omp_get_wtime  !OpenMP routine

    integer, parameter :: nmax = 1000
    integer, parameter :: nframes = 11

    !build the colormap:
    allocate(colormap(3,0:255))
    do i=0,255
        !colormap(:,i) = [255-i, 255-i, 255-i]  !gray
        colormap(:,i) = [255, 255-i, 255-i]     !red
    end do
    colormap(:,255) = [0,0,0] !black

    !full set:
 !   xmin = -2.0_wp
 !   xmax = 1.0_wp
 !   ymin = -1.3_wp
 !   ymax = 1.3_wp

    !zoom test:
    xmin = -0.55_wp
    xmax = 0.25_wp
    ymin = 0.5_wp
    ymax = 1.3_wp

    width = 250 !500 !1000
    height = width * ((ymax-ymin)/(xmax-xmin))

    xstep = (xmax-xmin)/width
    ystep = (ymax-ymin)/height

    allocate(pixel(nframes,width,height))

    tstart = omp_get_wtime()

    do iframe=1,nframes

        !if (nframes==1) then
        !    maxiter = nmax
        !else
        !    maxiter = 10 + (iframe-1)*(nmax/nframes)
        !end if

        maxiter = nmax

        !$omp parallel shared (pixel) private (ix,iy,x,y,c,z,n)
        !$omp do

        do ix=1,width

            x = xmin + (ix-1)*xstep

            do iy=1,height

                y = ymin + (iy-1)*ystep
                c = cmplx(x, y, kind=wp)

                z = 0
                n = 0
                do while (abs(z)<2.0_wp .and. (n<=maxiter) )
                  z = z*z + c
                  n = n + 1
                end do
                if (n>=maxiter) then
                    pixel(iframe,ix,height-iy+1) = 255         !in set
                else
                    !pixel(iframe,ix,height-iy+1) = mod(n,255)  !not in set
                    pixel(iframe,ix,height-iy+1) = (nmax*n)/(255)  !not in set

                end if

            end do    !ix
        end do    !iy

        !$omp end do
        !$omp end parallel

        !...zoom in test...
        tmp   = abs(xmax-xmin)
        xmin  = xmin + tmp/4
        xmax  = xmax - tmp/4
        tmp   = abs(ymax-ymin)
        ymin  = ymin + tmp/4
        ymax  = ymax - tmp/4
        xstep = (xmax-xmin)/width
        ystep = (ymax-ymin)/height

    end do    !iframe

    tend = omp_get_wtime()

    write(*,*) 'run time:',tend-tstart,' sec'

    call write_animated_gif('mandelbrot.gif',pixel,colormap,delay=10)

    end program mandelbrot_example
!*****************************************************************************************