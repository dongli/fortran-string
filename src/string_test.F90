program string_test

  use unit_test
  use string

  implicit none

  call test_suite_init('String test')

  call test_string_type()

  call test_string_replace()

  call test_string_delete()

  call test_string_split()

  call test_string_basename()

  call test_string_to_string()

  call test_suite_report()

  call test_suite_final()

contains

  subroutine test_string_type()

    type(string_type) s
    character(:), allocatable :: a

    call test_case_create('String type')

    s = 'abc'
    call assert_equal(len(s%value), 3, __FILE__, __LINE__)
    call assert_equal(s%value, 'abc', __FILE__, __LINE__)
    call assert_equal(s%at(1), 'a', __FILE__, __LINE__)

    a = s
    call assert_equal(len(a), 3, __FILE__, __LINE__)
    call assert_equal(a, 'abc', __FILE__, __LINE__)

    a = s // 'def'
    call assert_equal(len(a), 6, __FILE__, __LINE__)
    call assert_equal(a, 'abcdef', __FILE__, __LINE__)

    a = 'def' // s
    call assert_equal(len(a), 6, __FILE__, __LINE__)
    call assert_equal(a, 'defabc', __FILE__, __LINE__)

  end subroutine test_string_type

  subroutine test_string_replace()

    call test_case_create('String replace')

    call assert_equal(replace_string('data.txt', '.txt', ''), 'data', __FILE__, __LINE__)

  end subroutine test_string_replace

  subroutine test_string_delete()

    call test_case_create('String delete')

    call assert_equal(delete_string('data.nc', '.nc'), 'data', __FILE__, __LINE__)

  end subroutine test_string_delete

  subroutine test_string_split()

    type(string_type), allocatable :: fields(:)

    call test_case_create('String split')

    call assert_equal(count_string('a,b,d,d', ','), 3, __FILE__, __LINE__)
    call assert_equal(count_string('a//b//c', '//'), 2, __FILE__, __LINE__)

    fields = split_string('abc,,', ',')
    call assert_equal(size(fields), 2)
    call assert_equal(fields(1)%len(), 3, __FILE__, __LINE__)
    call assert_equal(fields(2)%len(), 0, __FILE__, __LINE__)
    call assert_equal(fields(1)%value, 'abc', __FILE__, __LINE__)
    call assert_equal(fields(2)%value, '', __FILE__, __LINE__)
    deallocate(fields)

    fields = split_string('/foo/bar/file', '/')
    call assert_equal(size(fields), 3)
    call assert_equal(fields(1)%len(), 3, __FILE__, __LINE__)
    call assert_equal(fields(2)%len(), 3, __FILE__, __LINE__)
    call assert_equal(fields(3)%len(), 4, __FILE__, __LINE__)
    call assert_equal(fields(1)%value, 'foo', __FILE__, __LINE__)
    call assert_equal(fields(2)%value, 'bar', __FILE__, __LINE__)
    call assert_equal(fields(3)%value, 'file', __FILE__, __LINE__)
    deallocate(fields)

  end subroutine test_string_split

  subroutine test_string_basename()

    call test_case_create('Basename')

    call assert_equal(basename('/foo/bar/file'), 'file', __FILE__, __LINE__)

  end subroutine test_string_basename

  subroutine test_string_to_string()

    call test_case_create('To string')

    call assert_equal(to_string(1), '1', __FILE__, __LINE__)
    call assert_equal(len(to_string(1)), 1, __FILE__, __LINE__)

    call assert_equal(to_string(-2), '-2', __FILE__, __LINE__)
    call assert_equal(len(to_string(-2)), 2, __FILE__, __LINE__)

    call assert_equal(to_string(.true.), 'true', __FILE__, __LINE__)
    call assert_equal(len(to_string(.true.)), 4, __FILE__, __LINE__)

    call assert_equal(to_string(1.245343, 4, 2), '1.25', __FILE__, __LINE__)
    call assert_equal(len(to_string(1.245343, 4, 2)), 4, __FILE__, __LINE__)

    call assert_equal(to_string([1.0d0, 2.0d0, 3.0d0], 3, 1), '[1.0,2.0,3.0]', __FILE__, __LINE__)
    call assert_equal(len(to_string([1.0d0, 2.0d0, 3.0d0], 3, 1)), 13, __FILE__, __LINE__)

  end subroutine test_string_to_string

end program string_test
