module dice_datamode_meps_mod

  use ESMF             , only : ESMF_SUCCESS, ESMF_LogWrite, ESMF_LOGMSG_INFO
  use ESMF             , only : ESMF_State, ESMF_StateGet, ESMF_MeshGet
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8
  use dshr_strdata_mod , only : shr_strdata_get_stream_pointer, shr_strdata_type
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add

  implicit none
  private

  public :: dice_datamode_meps_advertise
  public :: dice_datamode_meps_init_pointers
  public :: dice_datamode_meps_advance

  ! export state pointers
  real(r8), pointer :: Si_thick(:) => null()
  real(r8), pointer :: Si_ifrac(:) => null()

  ! stream data pointers
  real(r8), pointer :: strm_Si_thick(:) => null()
  real(r8), pointer :: strm_Si_ifrac(:) => null()

  character(len=*), parameter :: nullstr = 'null'
  character(len=*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine dice_datamode_meps_advertise(exportState, fldsexport, flds_scalar_name, rc)

    ! input/output variables
    type(esmf_State)   , intent(inout) :: exportState
    type(fldlist_type) , pointer       :: fldsexport
    character(len=*)   , intent(in)    :: flds_scalar_name
    integer            , intent(out)   :: rc

    ! local variables
    type(fldlist_type), pointer :: fldList
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call dshr_fldList_add(fldsExport, trim(flds_scalar_name))
    call dshr_fldList_add(fldsExport, 'Si_thick')
    call dshr_fldList_add(fldsExport, 'Si_ifrac')

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(dice_comp_advertise): Fr_ice'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine dice_datamode_meps_advertise

  !===============================================================================
  subroutine dice_datamode_meps_init_pointers(exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(dice_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! initialize export state pointers
    call dshr_state_getfldptr(exportState, 'Si_thick', fldptr1=Si_thick, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Si_ifrac', fldptr1=Si_ifrac, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! initialize stream pointers
    call shr_strdata_get_stream_pointer( sdat, 'Si_thick' , strm_Si_thick  , requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Si_ice_thick must be associated for meps datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Si_ifrac' , strm_Si_ifrac  , requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Si_ifrac must be associated for meps datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine dice_datamode_meps_init_pointers

  !===============================================================================
  subroutine dice_datamode_meps_advance(rc)

    ! input/output variables
    integer, intent(out)   :: rc

    ! local variables
    integer :: n
    integer :: lsize
    character(len=*), parameter :: subname='(dice_datamode_meps): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    lsize = size(Si_thick)

    do n = 1,lsize
       ! Set export fields as copies directly from streams
       Si_thick(n) = strm_Si_thick(n)
       Si_ifrac(n) = strm_Si_ifrac(n)
    enddo

  end subroutine dice_datamode_meps_advance

end module dice_datamode_meps_mod

  !===============================================================================
  subroutine dice_datamode_meps_restart_write(rpfile, case_name, inst_suffix, ymd, tod, &
       logunit, my_task, sdat, rc)

    ! input/output variables
    character(len=*)            , intent(in)    :: rpfile
    character(len=*)            , intent(in)    :: case_name
    character(len=*)            , intent(in)    :: inst_suffix
    integer                     , intent(in)    :: ymd       ! model date
    integer                     , intent(in)    :: tod       ! model sec into model date
    integer                     , intent(in)    :: logunit
    integer                     , intent(in)    :: my_task
    type(shr_strdata_type)      , intent(in)    :: sdat
    integer                     , intent(out)   :: rc
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call dshr_restart_write(rpfile, case_name, 'dice', inst_suffix, ymd, tod, &
         logunit, my_task, sdat, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine dice_datamode_meps_restart_write

  !===============================================================================
  subroutine dice_datamode_meps_restart_read(rest_filem, rpfile, logunit, my_task, mpicom, sdat, rc)

    ! input/output arguments
    character(len=*)            , intent(inout) :: rest_filem
    character(len=*)            , intent(in)    :: rpfile
    integer                     , intent(in)    :: logunit
    integer                     , intent(in)    :: my_task
    integer                     , intent(in)    :: mpicom
    type(shr_strdata_type)      , intent(in)    :: sdat
    integer                     , intent(out)   :: rc
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! read restart
    call dshr_restart_read(rest_filem, rpfile, logunit, my_task, mpicom, sdat, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

 end subroutine dice_datamode_meps_restart_read
