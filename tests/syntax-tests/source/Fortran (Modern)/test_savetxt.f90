program test_savetxt
use stdlib_kinds, only: int32, sp, dp
use stdlib_io, only: loadtxt, savetxt
use stdlib_error, only: check
implicit none

character(:), allocatable :: outpath

outpath = get_outpath() // "/tmp.dat"

call test_iint32(outpath)
call test_rsp(outpath)
call test_rdp(outpath)
call test_csp(outpath)
call test_cdp(outpath)

contains

    function get_outpath() result(outpath)
    integer :: ierr
    character(256) :: argv
    character(:), allocatable :: outpath

    call get_command_argument(1, argv, status=ierr)
    if (ierr==0) then
        outpath = trim(argv)
    else
        outpath = '.'
    endif
    end function get_outpath

    subroutine test_iint32(outpath)
    character(*), intent(in) :: outpath
    integer(int32) :: d(3, 2), e(2, 3)
    integer(int32), allocatable :: d2(:, :)
    d = reshape([1, 2, 3, 4, 5, 6], [3, 2])
    call savetxt(outpath, d)
    call loadtxt(outpath, d2)
    call check(all(shape(d2) == [3, 2]))
    call check(all(abs(d-d2) == 0))

    e = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    call savetxt(outpath, e)
    call loadtxt(outpath, d2)
    call check(all(shape(d2) == [2, 3]))
    call check(all(abs(e-d2) == 0))
    end subroutine


    subroutine test_rsp(outpath)
    character(*), intent(in) :: outpath
    real(sp) :: d(3, 2), e(2, 3)
    real(sp), allocatable :: d2(:, :)
    d = reshape([1, 2, 3, 4, 5, 6], [3, 2])
    call savetxt(outpath, d)
    call loadtxt(outpath, d2)
    call check(all(shape(d2) == [3, 2]))
    call check(all(abs(d-d2) < epsilon(1._sp)))

    e = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    call savetxt(outpath, e)
    call loadtxt(outpath, d2)
    call check(all(shape(d2) == [2, 3]))
    call check(all(abs(e-d2) < epsilon(1._sp)))
    end subroutine test_rsp


    subroutine test_rdp(outpath)
    character(*), intent(in) :: outpath
    real(dp) :: d(3, 2), e(2, 3)
    real(dp), allocatable :: d2(:, :)
    d = reshape([1, 2, 3, 4, 5, 6], [3, 2])
    call savetxt(outpath, d)
    call loadtxt(outpath, d2)
    call check(all(shape(d2) == [3, 2]))
    call check(all(abs(d-d2) < epsilon(1._dp)))

    e = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    call savetxt(outpath, e)
    call loadtxt(outpath, d2)
    call check(all(shape(d2) == [2, 3]))
    call check(all(abs(e-d2) < epsilon(1._dp)))
    end subroutine test_rdp

    subroutine test_csp(outpath)
    character(*), intent(in) :: outpath
    complex(sp) :: d(3, 2), e(2, 3)
    complex(sp), allocatable :: d2(:, :)
    d = cmplx(1, 1,kind=sp)* reshape([1, 2, 3, 4, 5, 6], [3, 2])
    call savetxt(outpath, d)
    call loadtxt(outpath, d2)
    call check(all(shape(d2) == [3, 2]))
    call check(all(abs(d-d2) < epsilon(1._sp)))

    e = cmplx(1, 1,kind=sp)* reshape([1, 2, 3, 4, 5, 6], [2, 3])
    call savetxt(outpath, e)
    call loadtxt(outpath, d2)
    call check(all(shape(d2) == [2, 3]))
    call check(all(abs(e-d2) < epsilon(1._sp)))
    end subroutine test_csp

    subroutine test_cdp(outpath)
    character(*), intent(in) :: outpath
    complex(dp) :: d(3, 2), e(2, 3)
    complex(dp), allocatable :: d2(:, :)
    d = cmplx(1._dp, 1._dp,kind=dp)* reshape([1, 2, 3, 4, 5, 6], [3, 2])
    call savetxt(outpath, d)
    call loadtxt(outpath, d2)
    call check(all(shape(d2) == [3, 2]))
    call check(all(abs(d-d2) < epsilon(1._dp)))

    e = cmplx(1, 1,kind=dp)* reshape([1, 2, 3, 4, 5, 6], [2, 3])
    call savetxt(outpath, e)
    call loadtxt(outpath, d2)
    call check(all(shape(d2) == [2, 3]))
    call check(all(abs(e-d2) < epsilon(1._dp)))
    end subroutine test_cdp

end program test_savetxt
