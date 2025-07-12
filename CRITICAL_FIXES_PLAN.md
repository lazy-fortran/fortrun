# Critical Fixes Implementation Plan

## Overview
Two critical issues need immediate attention:
1. Cache directory copy error (`cp: -r not specified`)
2. Inefficient .f file caching (no content-based hashing)

## Solution Design: Content-Based .f File Caching

### Core Concept
```
Original: .f file → project_dir_with_f90_extension/ → copy errors + bad caching
New:      .f file → content_hash.f90 → regular .f90 caching → clean & efficient
```

### Implementation Strategy

#### Phase 1: Simple .f Files (Clean Solution)
```fortran
! Current problematic flow:
source.f → preprocess → cache/source_simple_source.f90/ → FMP project → errors

! New clean flow:  
source.f → content_hash → cache/preprocessed_ABC123.f90 → treat as .f90 file
```

**Benefits:**
- No more directory naming conflicts (no .f90 extensions on directories)
- Content-based cache invalidation works properly
- Reuses existing .f90 caching infrastructure
- Eliminates the copy error entirely

#### Phase 2: Notebook Mode Complexity
```fortran
! For notebook .f files:
notebook.f → parse cells → combine cell content → hash → cache/notebook_DEF456.f90
```

**Special considerations:**
- Cell execution state persistence
- Variable state between cells  
- Figure generation and capture
- Output rendering requirements

### Implementation Steps

#### Step 1: Fix Immediate Copy Error (Quick Fix)
```fortran
! In src/runner.f90, line 455-461:
! Change from:
command = 'find "' // trim(source_dir) // '" -maxdepth 1 -name "*.f90" -o -name "*.F90" | ...'

! To:
command = 'find "' // trim(source_dir) // '" -maxdepth 1 -type f \( -name "*.f90" -o -name "*.F90" \) | ...'
```

#### Step 2: Implement Content-Based Hashing for .f Files
```fortran
! New function in src/cache.f90:
function get_f_file_content_hash(file_path) result(hash)
    use fortplotlib, only: crc32  ! Use existing CRC32 from fortplotlib
    character(len=*), intent(in) :: file_path
    character(len=8) :: hash
    character(len=:), allocatable :: content
    integer :: crc_value
    
    ! Read .f file content
    call read_file_content(file_path, content)
    
    ! Generate CRC32 hash (much simpler than SHA256!)
    crc_value = crc32(content)
    
    ! Convert to hex string (e.g., "A1B2C3D4")
    write(hash, '(Z8.8)') crc_value
end function

! Modify src/runner.f90:
if (is_preprocessor_file(source_file)) then
    content_hash = get_f_file_content_hash(source_file)
    preprocessed_file = cache_dir // '/preprocessed_' // content_hash // '.f90'  ! e.g., preprocessed_A1B2C3D4.f90
    
    ! Check if already cached
    if (.not. file_exists(preprocessed_file)) then
        call preprocess_file(source_file, preprocessed_file)
    end if
    
    ! Now treat as regular .f90 file
    call run_f90_file(preprocessed_file, ...)
end if
```

#### Step 3: Handle Notebook Mode
```fortran
! For notebook .f files:
if (notebook_mode) then
    ! Parse cells and compute combined hash using CRC32
    combined_hash = get_notebook_content_hash(source_file)  ! Also uses crc32() internally
    notebook_f90 = cache_dir // '/notebook_' // combined_hash // '.f90'  ! e.g., notebook_B2C3D4E5.f90
    
    ! Special notebook preprocessing and execution
    call process_notebook_with_persistence(source_file, notebook_f90, ...)
else
    ! Regular .f file processing (Step 2)
end if
```

### Testing Strategy

1. **Test copy error fix**:
   ```bash
   # Create .f file and run - should see no "cp: -r" errors
   echo 'print *, "test"' > test.f
   fortran test.f 2>&1 | grep -i "cp.*-r"  # Should be empty
   ```

2. **Test content-based caching**:
   ```bash
   # First run - creates cache
   fortran test.f
   
   # Second run - should be cache hit (same hash)
   fortran test.f
   
   # Modify file - should create new cache entry
   echo 'print *, "modified"' > test.f
   fortran test.f  # Should see different cache directory
   ```

3. **Test notebook mode**:
   ```bash
   # Test notebook with cells
   fortran --notebook notebook.f
   ```

### Benefits of This Approach

1. **Eliminates copy errors**: No more directories with .f90 extensions
2. **Proper cache invalidation**: Content changes → new hash → new cache entry
3. **Reuses existing infrastructure**: Preprocessed .f90 files use existing .f90 caching
4. **Clean separation**: Simple .f files vs notebook .f files handled appropriately
5. **Performance**: True content-based caching like .f90 files

### Migration Strategy

1. **Backward compatibility**: Existing cache entries remain functional
2. **Gradual migration**: New .f files use new system, old cache entries age out naturally
3. **Cache cleanup**: Optional cleanup of old incorrectly-named cache directories

This solution addresses both issues with a clean, reliable approach that leverages existing infrastructure while providing proper content-based caching for .f files.