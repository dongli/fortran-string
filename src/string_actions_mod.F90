module string_actions_mod

  use string_mod

  implicit none

  private

  public count_string
  public split_string
  public pad_string
  public replace_string
  public delete_string
  public dirname
  public basename

  interface count_string
    module procedure count_string_1
  end interface count_string

  interface split_string
    module procedure split_string_1
    module procedure split_string_2
  end interface split_string

  interface replace_string
    module procedure replace_string_1
    module procedure replace_string_2
  end interface replace_string

  interface delete_string
    module procedure delete_string_1
  end interface delete_string

  interface dirname
    module procedure dirname_1
  end interface dirname

  interface basename
    module procedure basename_1
  end interface basename

contains

  pure integer function count_string_1(str, pattern) result(res)

    character(*), intent(in) :: str
    character(*), intent(in) :: pattern

    integer i, len_pat

    len_pat = len(pattern)
    res = 0
    i = 1
    do while (i <= len_trim(str))
      if (i + len_pat - 1 > len_trim(str)) exit
      if (str(i:i+len_pat-1) == pattern) then
        res = res + 1
        i = i + len_pat
      else
        i = i + 1
      end if
    end do

  end function count_string_1

  pure function split_string_1(str, delim) result(res)

    character(*), intent(in) :: str
    character(*), intent(in) :: delim
    type(string_type), allocatable :: res(:)

    integer num_field, len_field, len_sep
    integer i1, i2

    len_sep = len(delim)

    num_field = 0; i1 = 1; i2 = 1
    do while (i2 <= len_trim(str))
      if (str(i2:i2+len_sep-1) == delim) then
        if (i2 > 1) num_field = num_field + 1
        i2 = i2 + len_sep
        i1 = i2
      else if (i2 == len_trim(str)) then
        if (i1 > 0) num_field = num_field + 1
        exit
      else
        i2 = i2 + 1
      end if
    end do

    allocate(res(num_field))

    num_field = 0; i1 = 1; i2 = 1
    do while (i2 <= len_trim(str))
      if (str(i2:i2+len_sep-1) == delim) then
        if (i2 > 1) then
          num_field = num_field + 1
          res(num_field) = str(i1:i2-1)
        end if
        i2 = i2 + len_sep
        i1 = i2
      else if (i2 == len_trim(str)) then
        if (i1 > 0) then
          num_field = num_field + 1
          res(num_field) = str(i1:i2)
        end if
        exit
      else
        i2 = i2 + 1
      end if
    end do

  end function split_string_1

  pure function split_string_2(str, delim, index) result(res)

    character(*), intent(in) :: str
    character(*), intent(in) :: delim
    integer, intent(in) :: index
    character(:), allocatable :: res

    type(string_type), allocatable :: tmp(:)

    tmp = split_string_1(str, delim)

    res = trim(tmp(index)%value)

  end function split_string_2

  pure function pad_string(str, width) result(res)

    character(*), intent(in) :: str
    integer, intent(in) :: width
    character(width) res

    res = str

  end function pad_string

  pure function replace_string_1(str, pattern, replace) result(res)

    character(*), intent(in) :: str
    character(*), intent(in) :: pattern
    character(*), intent(in) :: replace
    character(:), allocatable :: res

    integer i

    i = index(str, pattern)

    allocate(character((len_trim(str)-len(pattern)+len_trim(replace)))::res)

    res = str(1:i-1) // trim(replace) // str(i+len(pattern):len_trim(str))

  end function replace_string_1

  pure function replace_string_2(str, pattern, replace) result(res)

    type(string_type), intent(in) :: str
    character(*), intent(in) :: pattern
    character(*), intent(in) :: replace
    character(:), allocatable :: res

    res = replace_string_1(str%value, pattern, replace)

  end function replace_string_2

  pure function delete_string_1(str, pattern) result(res)

    character(*), intent(in) :: str
    character(*), intent(in) :: pattern
    character(:), allocatable :: res

    res = replace_string(str, pattern, '')

  end function delete_string_1

  pure function dirname_1(file_path) result(res)

    character(*), intent(in) :: file_path
    character(:), allocatable :: res

    res = replace_string(file_path, basename(file_path), '')

  end function dirname_1

  pure function basename_1(file_path, ext) result(res)

    character(*), intent(in) :: file_path
    character(*), intent(in), optional :: ext
    character(:), allocatable :: res

    type(string_type), allocatable :: fields(:)

    fields = split_string(trim(file_path), '/')

    if (present(ext)) then
      res = replace_string(fields(size(fields)), trim(ext), '')
    else
      res = fields(size(fields))
    end if

    deallocate(fields)

  end function basename_1

end module string_actions_mod
