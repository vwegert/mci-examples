  " Makro zur Implementierung der Lesezugriffe
  DEFINE __get_value__.
    TRY.
        me->get_data(
          EXPORTING
            i_name  = &1
          IMPORTING
            e_value = r_result
        ).
      CATCH cx_ishmed_not_found.
        " bisher kein Wert gespeichert --> Initialwert zurückgeben
        FREE r_result.
    ENDTRY.
  END-OF-DEFINITION.

  " Makro zur Implementierung der Schreibzugriffe
  DEFINE __set_value__.
    TRY.
      me->set_data(
          i_name  = &1
          i_value = i_value
      ).
    CATCH cx_ishmed_not_supported INTO DATA(exception).
      " Ein Datentyp wird unerwarteterweise nicht mehr unterstützt
      RAISE EXCEPTION TYPE zcx_mcibuch_dynamic_check
        EXPORTING
          textid   = zcx_mcibuch_dynamic_check=>unsupported_type
          previous = exception.
    ENDTRY.
  END-OF-DEFINITION.
