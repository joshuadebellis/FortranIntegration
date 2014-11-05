!     
! File:   trapezoidal_rule.F95
! Author: josh
!
! Created on November 4, 2014, 7:19 PM
!
program trapezoidal_rule
    implicit none
    real :: a, b, h, answer
    integer :: n, k
    write(*,*), 'pick integral bounds a and b'
    read(*, *), a,b
    write(*,*), 'pick no of strips in approximation'
    read(*, *), n
    answer = 0
    ! calculate the strip size 
    h = real(b - a) / n   
    ! evaluate the endpoints
    answer = answer + (f(a) + f(b))
    ! evaluate the midpoints
    do k = 1, n
        answer = answer + 2 * f(a + k * h)
    end do
    answer = answer * ( h / 2 )
    write(*, *), 'the answer is ' , answer
contains
    function f(x)
        real :: f
        real :: x
        f = x ** 2
    end function f
end program trapezoidal_rule
