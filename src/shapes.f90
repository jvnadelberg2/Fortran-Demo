! =============================================================================
! shapes.f90
! Purpose: OOP demo with abstract base type, extensions, and polymorphism.
! Highlights: deferred bindings, SELECT TYPE, type-bound procedures.
! =============================================================================
module shapes
  use kinds
  implicit none
  private
  public :: shape, circle, rectangle, area_of_any

  ! Abstract base: requires area() and perimeter() in children.
  type, abstract :: shape
  contains
    procedure(area_ifc),  deferred :: area
    procedure(perim_ifc), deferred :: perimeter
  end type

  ! Abstract interfaces declare the binding signatures.
  abstract interface
    real(rk) function area_ifc(self)
      import :: shape, rk
      class(shape), intent(in) :: self
    end function
    real(rk) function perim_ifc(self)
      import :: shape, rk
      class(shape), intent(in) :: self
    end function
  end interface

  ! Concrete circle implements the bindings.
  type, extends(shape) :: circle
    real(rk) :: r   ! radius
  contains
    procedure :: area      => circle_area
    procedure :: perimeter => circle_perim
  end type

  ! Concrete rectangle implements the bindings.
  type, extends(shape) :: rectangle
    real(rk) :: w, h
  contains
    procedure :: area      => rect_area
    procedure :: perimeter => rect_perim
  end type

contains
  real(rk) function circle_area(self)
    class(circle), intent(in) :: self
    circle_area = acos(-1.0_rk) * self%r * self%r
  end function

  real(rk) function circle_perim(self)
    class(circle), intent(in) :: self
    circle_perim = 2.0_rk * acos(-1.0_rk) * self%r
  end function

  real(rk) function rect_area(self)
    class(rectangle), intent(in) :: self
    rect_area = self%w * self%h
  end function

  real(rk) function rect_perim(self)
    class(rectangle), intent(in) :: self
    rect_perim = 2.0_rk * (self%w + self%h)
  end function

  real(rk) function area_of_any(s)
    ! Polymorphic dispatch demo. SELECT TYPE branches on dynamic type.
    class(shape), intent(in) :: s
    select type (s)
    type is (circle)
      area_of_any = s%area()
    type is (rectangle)
      area_of_any = s%area()
    class default
      area_of_any = -1.0_rk
    end select
  end function
end module shapes
