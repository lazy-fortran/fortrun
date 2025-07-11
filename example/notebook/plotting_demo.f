! %% [markdown]
! # Plotting with Fortplotlib
! 
! This notebook demonstrates how plots are captured and embedded
! as inline images in the output.

! %%
use fortplot
implicit none

real(8) :: x(100), y(100), z(100)
integer :: i

! %% [markdown]
! ## Generate Data
! 
! First, let's create some data to plot:

! %%
! Create x values from 0 to 2*pi
do i = 1, 100
    x(i) = (i-1) * 2.0 * 3.14159 / 99.0
    y(i) = sin(x(i))
    z(i) = cos(x(i))
end do

print *, "Data generated for", size(x), "points"

! %% [markdown]
! ## Simple Line Plot
! 
! Create a basic sine wave plot:

! %%
call figure()
call plot(x, y)
call xlabel("x")
call ylabel("sin(x)")
call title("Sine Wave")
call show()  ! This will be converted to inline image

! %% [markdown]
! ## Multiple Series
! 
! Plot both sine and cosine on the same figure:

! %%
call figure()
call plot(x, y, label="sin(x)", linestyle="b-")
call plot(x, z, label="cos(x)", linestyle="r--")
call xlabel("x")
call ylabel("y")
call title("Trigonometric Functions")
call legend()
call grid(.true.)
call show()  ! Another inline image

! %% [markdown]
! ## Scatter Plot
! 
! Create a scatter plot with some random data:

! %%
real(8) :: rx(50), ry(50)

! Generate random-like data
do i = 1, 50
    rx(i) = real(i) / 10.0
    ry(i) = rx(i)**2 + 0.5 * sin(real(i))
end do

call figure(600, 400)
call scatter(rx, ry)
call xlabel("x")
call ylabel("y")
call title("Scatter Plot Example")
call show()

! %% [markdown]
! ## Summary
! 
! All `show()` calls in this notebook are automatically converted to
! inline PNG images encoded as base64 in the markdown output.
! This allows for a complete, self-contained document with embedded
! visualizations.