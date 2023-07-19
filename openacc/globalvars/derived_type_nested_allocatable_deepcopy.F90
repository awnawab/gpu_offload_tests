! (C) Copyright 2023- ECMWF.
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

module kernel_mod
contains
subroutine kernel(j,n)
use vars_mod_nested, only: v
implicit none

integer, intent(in) :: j,n
integer :: i
!$acc routine vector
!$acc data present(v)

!$acc loop vector
do i=1,n
   v%c(j)%x(i) = v%a(j)%x(i) + v%b(j)%x(i)
enddo

!$acc end data
end subroutine kernel
end module kernel_mod

program main
use vars_mod_nested, only: v
use kernel_mod, only: kernel
implicit none
integer :: i,j,n,m

n = 32
m = 128

allocate(v%a(m))
allocate(v%b(m))
allocate(v%c(m))
do j=1,m
   allocate(v%a(j)%x(n))
   allocate(v%b(j)%x(n))
   allocate(v%c(j)%x(n))
   v%c(j)%x = 0
   do i=1,n
     v%a(j)%x(i) = i
     v%b(j)%x(i) = 2*v%a(j)%x(i)
   enddo
enddo

!$acc enter data copyin(v)

!$acc parallel loop gang
do j=1,m
  call kernel(j,n)
enddo

!$acc exit data copyout(v)

do j=1,m
  do i=1,n
    if(v%c(j)%x(i) /= (v%a(j)%x(i) + v%b(j)%x(i)))then
       print *, i, v%c(j)%x(i), v%a(j)%x(i), v%b(j)%x(i)
       error stop
    endif
  enddo

  deallocate(v%a(j)%x)
  deallocate(v%b(j)%x)
  deallocate(v%c(j)%x)
enddo
deallocate(v%a)
deallocate(v%b)
deallocate(v%c)

end program main
