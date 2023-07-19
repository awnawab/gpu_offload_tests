! (C) Copyright 2023- ECMWF.
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

#define m 128

module kernel_mod
contains
subroutine kernel(j,n)
use vars_mod, only: a,b,c
implicit none

integer, intent(in) :: j,n
integer :: i
!$acc routine vector
!$acc data present(a,b,c)

!$acc loop vector
do i=1,n
   c(j)%x(i) = a(j)%x(i) + b(j)%x(i)
enddo

!$acc end data
end subroutine kernel
end module kernel_mod

program main
use vars_mod, only: a,b,c
use kernel_mod, only: kernel
implicit none
integer :: i,j,n

n = 32
do j=1,m
   allocate(a(j)%x(n))
   allocate(b(j)%x(n))
   allocate(c(j)%x(n))
   c(j)%x = 0
   do i=1,n
     a(j)%x(i) = i
     b(j)%x(i) = 2*a(j)%x(i)
   enddo
enddo

!$acc enter data copyin(a,b,c)

!$acc parallel loop gang
do j=1,m
  call kernel(j,n)
enddo

!$acc exit data copyout(c)
!$acc exit data delete(a,b)

do j=1,m
  do i=1,n
    if(c(j)%x(i) /= (a(j)%x(i) + b(j)%x(i)))then
       print *, i, c(j)%x(i), a(j)%x(i), b(j)%x(i)
       error stop
    endif
  enddo

  deallocate(a(j)%x)
  deallocate(b(j)%x)
  deallocate(c(j)%x)
enddo

end program main
