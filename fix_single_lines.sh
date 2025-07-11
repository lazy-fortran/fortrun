#\!/bin/bash

# Fix single-line module definitions
sed -i 's/echo "module shared; integer :: x = 42; end module"/cat <<EOL > shared.f90\nmodule shared\n    integer :: x = 42\nend module\nEOL/' test/test_module_cache_final.f90

sed -i 's/echo "module math_package; real :: pi = 3.14159; end module"/cat <<EOL > math_package.f90\nmodule math_package\n    real :: pi = 3.14159\nend module\nEOL/' test/test_module_cache_final.f90

# Fix single-line program definitions
sed -i 's/echo "program test; print \*, \\"basic test\\"; end program"/cat <<EOL > test_basic.f90\nprogram test\n    print *, "basic test"\nend program\nEOL/' test/test_module_cache_final.f90

sed -i 's/echo "program app1; use shared; print \*, x; end program"/cat <<EOL > app1.f90\nprogram app1\n    use shared\n    print *, x\nend program\nEOL/' test/test_module_cache_final.f90

sed -i 's/echo "program app2; use shared; print \*, x + 1; end program"/cat <<EOL > app2.f90\nprogram app2\n    use shared\n    print *, x + 1\nend program\nEOL/' test/test_module_cache_final.f90

sed -i 's/echo "program test; print \*, \\"structure test\\"; end program"/cat <<EOL > test_structure.f90\nprogram test\n    print *, "structure test"\nend program\nEOL/' test/test_module_cache_final.f90

sed -i 's/echo "program test; print \*, \\"cache test\\"; end program"/cat <<EOL > test_cache.f90\nprogram test\n    print *, "cache test"\nend program\nEOL/' test/test_module_cache_final.f90

sed -i 's/echo "program calc1; use math_package; print \*, pi; end program"/cat <<EOL > calc1.f90\nprogram calc1\n    use math_package\n    print *, pi\nend program\nEOL/' test/test_module_cache_final.f90

sed -i 's/echo "program calc2; use math_package; print \*, pi \* 2; end program"/cat <<EOL > calc2.f90\nprogram calc2\n    use math_package\n    print *, pi * 2\nend program\nEOL/' test/test_module_cache_final.f90
