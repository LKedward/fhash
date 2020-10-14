! TestLite: A lightweight testing framework
!
! MIT License
!
! Copyright (c) 2020 fpm contributors
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.

module TestLite
  use, intrinsic :: iso_fortran_env, only : error_unit
  use TestLite_suite, only : run_testsuite, new_testsuite, testsuite_t, &
        & select_suite, run_selected
  implicit none

  contains

  !> Run a collection of testsuites
  subroutine run_tests(testsuite)
    type(testsuite_t), intent(in) :: testsuite(:)

    integer :: stat, is
    character(len=:), allocatable :: suite_name, test_name
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0
    
    call get_argument(1, suite_name)
    call get_argument(2, test_name)

    if (allocated(suite_name)) then
        is = select_suite(testsuite, suite_name)
        if (is > 0 .and. is <= size(testsuite)) then
            if (allocated(test_name)) then
                write(error_unit, fmt) "Suite:", testsuite(is)%name
                call run_selected(testsuite(is)%collect, test_name, error_unit, stat)
                if (stat < 0) then
                    error stop 1
                end if
            else
                write(error_unit, fmt) "Testing:", testsuite(is)%name
                call run_testsuite(testsuite(is)%collect, error_unit, stat)
            end if
        else
            write(error_unit, fmt) "Available testsuites"
            do is = 1, size(testsuite)
                write(error_unit, fmt) "-", testsuite(is)%name
            end do
            error stop 1
        end if
    else
        do is = 1, size(testsuite)
            write(error_unit, fmt) "Testing:", testsuite(is)%name
            call run_testsuite(testsuite(is)%collect, error_unit, stat)
        end do
    end if

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop 1
    end if

  end subroutine run_tests


  !> Obtain the command line argument at a given index
  subroutine get_argument(idx, arg)

      !> Index of command line argument, range [0:command_argument_count()]
      integer, intent(in) :: idx

      !> Command line argument
      character(len=:), allocatable, intent(out) :: arg

      integer :: length, stat

      call get_command_argument(idx, length=length, status=stat)
      if (stat /= 0) then
          return
      endif

      allocate(character(len=length) :: arg, stat=stat)
      if (stat /= 0) then
          return
      endif

      if (length > 0) then
          call get_command_argument(idx, arg, status=stat)
          if (stat /= 0) then
              deallocate(arg)
              return
          end if
      end if

  end subroutine get_argument


end module TestLite