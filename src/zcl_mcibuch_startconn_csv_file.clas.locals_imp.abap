CLASS lcl_file_access IMPLEMENTATION.

  METHOD zif_mcibuch_file_access~open_dataset.
    ASSERT me->filename IS INITIAL.
    OPEN DATASET i_filename FOR INPUT IN TEXT MODE ENCODING UTF-8.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_mcibuch_file_not_found .
    ENDIF.
    me->filename = i_filename.
  ENDMETHOD.

  METHOD zif_mcibuch_file_access~read_dataset.
    ASSERT me->filename IS NOT INITIAL.
    READ DATASET me->filename INTO r_result.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_mcibuch_eof_reached.
    ENDIF.
  ENDMETHOD.

  METHOD zif_mcibuch_file_access~close_dataset.
    ASSERT me->filename IS NOT INITIAL.
    CLOSE DATASET me->filename.
  ENDMETHOD.

  METHOD zif_mcibuch_file_access~delete_dataset.
    ASSERT me->filename IS NOT INITIAL.
    DELETE DATASET me->filename.
    CLEAR me->filename.
  ENDMETHOD.

ENDCLASS.
