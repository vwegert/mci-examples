*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_bapi_wrapper IMPLEMENTATION.

  METHOD constructor.
    me->destination = i_destination.
  ENDMETHOD.

  METHOD zif_mcibuch_visit_bapi_wrapper~call_patcase_getoutpatvisit.

    FREE: es_visit_data,
          es_visit_data_at,
          e_worst_returned_msgty,
          et_messages.

    CALL FUNCTION 'BAPI_PATCASE_GETOUTPATVISIT' DESTINATION me->destination ##enh_ok
      EXPORTING
        institution          = i_institution
        patcaseid            = i_case_id
        movemnt_seqno        = i_movement_number
      IMPORTING
        outpat_visit_data    = es_visit_data
        outpat_visit_data_at = es_visit_data_at
        worst_returned_msgty = e_worst_returned_msgty
      TABLES
        return               = et_messages.

  ENDMETHOD.

  METHOD zif_mcibuch_visit_bapi_wrapper~call_patcase_addoutpatvisit.

    FREE: es_new_case_data,
          es_new_movement_data,
          es_new_movement_data_at,
          e_worst_returned_msgty,
          et_messages.

    CALL FUNCTION 'BAPI_PATCASE_ADDOUTPATVISIT' DESTINATION me->destination ##enh_ok
      EXPORTING
        client                = sy-mandt
        institution           = i_institution
        patcaseid             = i_case_id
        outpat_visit_data     = is_outpat_visit_data
        outpat_visit_data_med = is_outpat_visit_data_med
        outpat_visit_data_at  = is_outpat_visit_data_at
        testrun               = i_testrun
      IMPORTING
        new_patcase_data      = es_new_case_data
        new_movemnt_data      = es_new_movement_data
        new_movemnt_data_at   = es_new_movement_data_at
        worst_returned_msgty  = e_worst_returned_msgty
      TABLES
        return                = et_messages.

  ENDMETHOD.

  METHOD zif_mcibuch_visit_bapi_wrapper~call_patcase_changeoutpatvisit.

    FREE: es_changed_case_data,
          es_changed_movement_data,
          es_changed_movement_data_at,
          e_worst_returned_msgty,
          et_messages.

    CALL FUNCTION 'BAPI_PATCASE_CHANGEOUTPATVISIT' DESTINATION me->destination ##enh_ok
      EXPORTING
        client                = sy-mandt
        institution           = i_institution
        patcaseid             = i_case_id
        movemnt_seqno         = i_movement_number
        outpat_visit_data     = is_changed_visit_data
        outpat_visit_data_med = is_changed_visit_data_med
        outpat_visit_data_at  = is_changed_visit_data_at
        testrun               = i_testrun
      IMPORTING
        new_patcase_data      = es_changed_case_data
        new_movemnt_data      = es_changed_movement_data
        new_movemnt_data_at   = es_changed_movement_data_at
        worst_returned_msgty  = e_worst_returned_msgty
      TABLES
        return                = et_messages.

  ENDMETHOD.

  METHOD zif_mcibuch_visit_bapi_wrapper~call_patcase_canceloutpatvisit.

    FREE: e_worst_returned_msgty,
          et_messages.

    CALL FUNCTION 'BAPI_PATCASE_CANCELOUTPATVISIT' DESTINATION me->destination ##enh_ok
      EXPORTING
        client               = sy-mandt
        institution          = i_institution
        patcaseid            = i_case_id
        movemnt_seqno        = i_movement_number
        cancel_user_date     = is_cancel_user_date
        testrun              = i_testrun
      IMPORTING
        worst_returned_msgty = e_worst_returned_msgty
      TABLES
        return               = et_messages.

  ENDMETHOD.

  METHOD zif_mcibuch_visit_bapi_wrapper~call_transaction_commit.

    FREE: es_message.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' DESTINATION me->destination
      EXPORTING
        wait   = i_wait
      IMPORTING
        return = es_message.

  ENDMETHOD.

  METHOD zif_mcibuch_visit_bapi_wrapper~call_transaction_rollback.

    FREE: es_message.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' DESTINATION me->destination
      IMPORTING
        return = es_message.

  ENDMETHOD.

ENDCLASS.
