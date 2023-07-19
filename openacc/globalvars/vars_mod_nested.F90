! (C) Copyright 2023- ECMWF.
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

module vars_mod_nested
  type :: container
     integer, allocatable :: x(:)
  end type container

  type :: bucket
     type(container), allocatable :: a(:), b(:), c(:)
  end type bucket

  type(bucket) :: v
!$acc declare create(v)
end module vars_mod_nested
