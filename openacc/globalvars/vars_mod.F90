! (C) Copyright 2023- ECMWF.
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

#define m 128

module vars_mod
  type :: container
     integer, allocatable :: x(:)
  end type container
  type(container) :: a(m), b(m), c(m)

!$acc declare create(a,b,c)
end module vars_mod
