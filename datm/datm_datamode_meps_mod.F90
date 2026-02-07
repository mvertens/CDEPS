module datm_datamode_meps_mod

  use ESMF             , only : ESMF_SUCCESS, ESMF_LogWrite, ESMF_LOGMSG_INFO
  use ESMF             , only : ESMF_State, ESMF_StateGet, ESMF_MeshGet
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8
  use dshr_strdata_mod , only : shr_strdata_get_stream_pointer, shr_strdata_type
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add

  implicit none
  private

  public :: datm_datamode_meps_advertise
  public :: datm_datamode_meps_init_pointers
  public :: datm_datamode_meps_advance

  ! export state pointers
  real(r8), pointer :: Sa_u10m(:) => null()
  real(r8), pointer :: Sa_v10m(:) => null()

  ! stream data pointers
  real(r8), pointer :: strm_Sa_u10m(:) => null()
  real(r8), pointer :: strm_Sa_v10m(:) => null()

  character(len=*), parameter :: nullstr = 'null'
  character(len=*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine datm_datamode_meps_advertise(exportState, fldsexport, flds_scalar_name, rc)

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
    call dshr_fldList_add(fldsExport, 'Sa_u10m')
    call dshr_fldList_add(fldsExport, 'Sa_v10m')

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(datm_comp_advertise): Fr_atm'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine datm_datamode_meps_advertise

  !===============================================================================
  subroutine datm_datamode_meps_init_pointers(exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(datm_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! initialize export state pointers
    call dshr_state_getfldptr(exportState, 'Sa_u10m', fldptr1=Sa_u10m, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_v10m', fldptr1=Sa_v10m, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! initialize stream pointers
    call shr_strdata_get_stream_pointer( sdat, 'Sa_u10m' , strm_Sa_u10m  , requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Sa_u10m must be associated for meps datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Sa_v10m' , strm_Sa_v10m  , requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Sa_u10m must be associated for meps datamode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine datm_datamode_meps_init_pointers

  !===============================================================================
  subroutine datm_datamode_meps_advance(rc)

    ! input/output variables
    integer, intent(out)   :: rc

    ! local variables
    integer :: n
    integer :: lsize
    character(len=*), parameter :: subname='(datm_datamode_meps): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    lsize = size(Sa_u10m)

    do n = 1,lsize
       ! Set export fields as copies directly from streams
       Sa_u10m(n) = strm_Sa_u10m(n)
       Sa_v10m(n) = strm_Sa_v10m(n)
    enddo

  end subroutine datm_datamode_meps_advance

end module datm_datamode_meps_mod
