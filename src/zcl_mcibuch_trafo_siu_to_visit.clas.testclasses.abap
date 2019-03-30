**
** Mock Message Context
**
CLASS lcl_context DEFINITION
  INHERITING FROM cl_ishmed_mci_message_context
  FINAL CREATE PUBLIC.
ENDCLASS.

CLASS lcl_context IMPLEMENTATION.
ENDCLASS.

**
** Mock Runtime Parameters
**
CLASS lcl_runtime_params DEFINITION
  INHERITING FROM cl_ishmed_mci_runtime_params
  FINAL CREATE PUBLIC.
ENDCLASS.

CLASS lcl_runtime_params IMPLEMENTATION.
ENDCLASS.

**
** Local Subclass with Data Access Stubs
**
CLASS lcl_transformer DEFINITION
  INHERITING FROM zcl_mcibuch_trafo_siu_to_visit
  FINAL CREATE PUBLIC.

  PROTECTED SECTION.
    METHODS get_movement_type REDEFINITION.
    METHODS get_movement_status REDEFINITION.

ENDCLASS.

CLASS lcl_transformer IMPLEMENTATION.

  METHOD get_movement_type.
    r_result = 'X1'.
  ENDMETHOD.

  METHOD get_movement_status.
    r_result = 'X2'.
  ENDMETHOD.

ENDCLASS.

**
** Actual Unit Test
**
CLASS lcl_unit_test DEFINITION
  FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA errorhandler TYPE REF TO cl_ishmed_errorhandling.
    DATA logger TYPE REF TO if_ishmed_mci_logger.
    DATA context TYPE REF TO if_ishmed_mci_message_context.
    DATA runtime_params TYPE REF TO if_ishmed_mci_runtime_params.
    DATA transformer TYPE REF TO if_ishmed_mci_transformer.

    METHODS setup
      RAISING
        cx_ishmed_mci.
    METHODS teardown
      RAISING
        cx_ishmed_object.

    METHODS assert_error_message.
    METHODS assert_no_error_message.

    METHODS create_hl7_message_from_text
      IMPORTING
        i_message       TYPE stringtab
      RETURNING
        VALUE(r_result) TYPE REF TO if_ishmed_mci_message
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    METHODS cast_result_message
      IMPORTING
        i_message       TYPE REF TO if_ishmed_mci_message
      RETURNING
        VALUE(r_result) TYPE REF TO zcl_mcibuch_msg_case_visit.

    "! Dieser Test stellt sicher, dass der Transformer bei einer eingehenden
    "! Nachricht eines falschen Typs die Verarbeitung mit einer Ausnahme abbricht
    "! oder einen Fehlerstatus setzt.
    METHODS wrong_incoming_type_handling FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test stellt sicher, dass der Transformer bei einer eingehenden
    "! Nachricht einer falschen HL7-Struktur die Verarbeitung mit einer Ausnahme
    "! abbricht oder einen Fehlerstatus setzt.
    METHODS wrong_incoming_hl7_handling FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test stellt eine 'Grundlinie' dar: Eine einfache valide Nachricht
    "! ohne weitere Besonderheiten muss korrekt übersetzt werden.
    METHODS basic_valid_message FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test prüft das Verhalten bei Eingang einer fehlerhaften Nachricht
    "! (fehlendes SCH-Segment).
    METHODS missing_segment_sch FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test prüft das Verhalten bei Eingang einer fehlerhaften Nachricht
    "! (fehlendes TQ1-Segment).
    METHODS missing_segment_tq1 FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test prüft das Verhalten bei Eingang einer fehlerhaften Nachricht
    "! (fehlendes PV1-Segment).
    METHODS missing_segment_pv1 FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test prüft das Verhalten bei Eingang einer fehlerhaften Nachricht
    "! (fehlendes AIL-Segment).
    METHODS missing_segment_ail FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test prüft die korrekte Behandlung numerischer Einrichtungen mit
    "! fehlenden führenden Nullen.
    METHODS alpha_conv_inst_numeric FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test prüft die korrekte Behandlung "kurzer" alphanumerischer
    "! Einrichtungen.
    METHODS alpha_conv_inst_alphanumeric FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test prüft die korrekte Behandlung numerischer Fallnummern mit
    "! fehlenden führenden Nullen.
    METHODS alpha_conv_case_numeric FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test stellt sicher, dass Änderungen spezifischer Daten
    "! (hier: Startdatum und -zeit)
    "! gegenüber dem Grundlinien-Test basic_valid_message korrekt behandelt werden.
    METHODS verify_fields_start_date_time FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test stellt sicher, dass Änderungen spezifischer Daten
    "! (hier: Endedatum und -zeit)
    "! gegenüber dem Grundlinien-Test basic_valid_message korrekt behandelt werden.
    METHODS verify_fields_end_date_time FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test stellt sicher, dass Änderungen spezifischer Daten
    "! (hier: Fachabteilung)
    "! gegenüber dem Grundlinien-Test basic_valid_message korrekt behandelt werden.
    METHODS verify_fields_dept_ou FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test stellt sicher, dass Änderungen spezifischer Daten
    "! (hier: pflegerische OE)
    "! gegenüber dem Grundlinien-Test basic_valid_message korrekt behandelt werden.
    METHODS verify_fields_nurs_ou FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test stellt sicher, dass Änderungen spezifischer Daten
    "! (hier: Behandlungsraum)
    "! gegenüber dem Grundlinien-Test basic_valid_message korrekt behandelt werden.
    METHODS verify_fields_room FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test stellt sicher, dass Änderungen spezifischer Daten
    "! (hier: externe Bewegungs-ID)
    "! gegenüber dem Grundlinien-Test basic_valid_message korrekt behandelt werden.
    METHODS verify_fields_ext_movement_id FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test stellt sicher, dass Änderungen spezifischer Daten
    "! (hier: behandelnder Arzt)
    "! gegenüber dem Grundlinien-Test basic_valid_message korrekt behandelt werden.
    METHODS verify_fields_att_phys FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test stellt sicher, dass die Erkennung stornierter Termine
    "! (hier: Wert cancelled)
    "! gegenüber dem Grundlinien-Test basic_valid_message korrekt behandelt werden.
    METHODS verify_cancel_flag_cancelled_l FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test stellt sicher, dass die Erkennung stornierter Termine
    "! (hier: Wert Cancelled)
    "! gegenüber dem Grundlinien-Test basic_valid_message korrekt behandelt werden.
    METHODS verify_cancel_flag_cancelled_m FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test stellt sicher, dass die Erkennung stornierter Termine
    "! (hier: Wert CANCELLED)
    "! gegenüber dem Grundlinien-Test basic_valid_message korrekt behandelt werden.
    METHODS verify_cancel_flag_cancelled_u FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test stellt sicher, dass die Erkennung stornierter Termine
    "! (hier: Wert deleted)
    "! gegenüber dem Grundlinien-Test basic_valid_message korrekt behandelt werden.
    METHODS verify_cancel_flag_deleted_l FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test stellt sicher, dass die Erkennung stornierter Termine
    "! (hier: Wert Deleted)
    "! gegenüber dem Grundlinien-Test basic_valid_message korrekt behandelt werden.
    METHODS verify_cancel_flag_deleted_m FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test stellt sicher, dass die Erkennung stornierter Termine
    "! (hier: Wert DELETED)
    "! gegenüber dem Grundlinien-Test basic_valid_message korrekt behandelt werden.
    METHODS verify_cancel_flag_deleted_u FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test stellt sicher, dass der Transformer auch bei Verarbeitung
    "! mehrerer Nachrichten in Folge korrekt arbeitet.
    METHODS multiple_messages_processing FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

ENDCLASS.

CLASS lcl_unit_test IMPLEMENTATION.

  METHOD setup.
    me->errorhandler = NEW cl_ishmed_errorhandling( ).
    me->logger = NEW cl_ishmed_mci_log_errorhandler(
        ir_errorhandler = me->errorhandler
    ).
    me->context = NEW lcl_context( ).
    me->runtime_params = NEW lcl_runtime_params(
        i_process_id   = '00042000DEADBEEF001234'
        i_process_name = 'ZZ_UNIT_TEST_PROCESS'
    ).
    me->transformer = NEW lcl_transformer( ).
    me->transformer->init(
        i_type             = if_ishmed_mci_comp_constant=>co_transformer
        ir_logger          = me->logger
    ).
  ENDMETHOD.

  METHOD teardown.
    me->transformer->finalize( ).
    FREE me->transformer.
    me->runtime_params->finalize( ).
    FREE me->runtime_params.
    FREE me->context.
    me->logger->finalize( ).
    FREE me->logger.
    FREE me->errorhandler.
  ENDMETHOD.

  METHOD assert_error_message.
    me->errorhandler->get_max_errortype(
      IMPORTING
        e_maxty         = DATA(worst_message_type)
    ).
    IF worst_message_type NA 'EAX'.
      cl_abap_unit_assert=>fail( msg  = 'Expected error message not logged'
                                 quit = if_aunit_constants=>method ).
    ENDIF.
  ENDMETHOD.

  METHOD assert_no_error_message.
    me->errorhandler->get_max_errortype(
      IMPORTING
        e_maxty         = DATA(worst_message_type)
    ).
    IF worst_message_type CA 'EAX'.
      cl_abap_unit_assert=>fail( msg  = 'Unexpected error message logged'
                                 quit = if_aunit_constants=>method ).
    ENDIF.
  ENDMETHOD.

  METHOD create_hl7_message_from_text.
    DATA(hl7_parser) = CAST if_ishmed_mci_hl7_parser(
                         NEW cl_ishmed_mci_hl7_pipe_parser( ) ).
    hl7_parser->init(
        i_type    = if_ishmed_mci_comp_constant=>co_parser
        ir_logger = me->logger
     ).
    DATA message_text TYPE string.
    CONCATENATE LINES OF i_message
      INTO message_text
      SEPARATED BY cl_abap_char_utilities=>cr_lf.
    r_result = hl7_parser->parse(
         i_value   = message_text
         ir_logger = me->logger
     ).
    hl7_parser->if_ishmed_object~finalize( ).
  ENDMETHOD.

  METHOD cast_result_message.
    cl_abap_unit_assert=>assert_not_initial(
        act = i_message
        msg = 'Empty message returned'
    ).
    TRY.
        r_result ?= i_message.
      CATCH cx_sy_move_cast_error INTO DATA(exception).
        cl_abap_unit_assert=>fail(
            msg    = 'Invalid message type returned'
            detail = exception->get_text( )
        ).
    ENDTRY.
  ENDMETHOD.

  METHOD wrong_incoming_type_handling.

    " Der Transformer benötigt eine HL7-Nachricht. Zum Test der Fehlerbehandlung
    " versorgen wir ihn mit einer Nachricht eines ganz anderen Typs.
    DATA(invalid_message) = CAST if_ishmed_mci_message(
      NEW cl_ishmed_mci_string_message( )
    ).

    " Nachricht transformieren
    TRY.
        transformer->transform(
          EXPORTING
            ir_logger            = me->logger
            ir_message_context   = me->context
            ir_runtime_parameter = me->runtime_params
          CHANGING
            cr_message           = invalid_message
        ).
      CATCH cx_ishmed_mci_transform cx_ishmed_mci.
        " Eine Ausnahme beendet die Verarbeitung, das ist eine mögliche
        " korrekte Reaktion
        RETURN.
    ENDTRY.

    " Wenn wir hier angekommen sind, wurde keine Ausnahme ausgelöst. In dem Fall
    " muss eine Fehlernachricht geloggt worden sein UND der Status muss auf
    " 'fehlerhaft' gesetzt sein.
    assert_error_message( ).
    IF me->context->get_status( )
       <> if_ishmed_mci_message_constant=>co_message_status_error.
      cl_abap_unit_assert=>fail(
          msg = 'Transformer has failed to set process status to "error".'
      ).
    ENDIF.

  ENDMETHOD.

  METHOD wrong_incoming_hl7_handling.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ISHMED|XT1.200|||20171108150531||ADT^A28^ADT_A05|123456|D|2.5.1|||AL|NE|US|UNICODE|EN^English^ISO639||` )
            ( `EVN||20171108150450||O|||` )
            ( `PID|1||0002001475^^^^PI||Mittwoch^Michaela^^^^^L^A^^^G~Mittwoch^^^^^^M^A||19600504|F|||||||EN^English^ISO639|||||||||||US^amerikanisch^ISO3166||||N|||||||||` )
            ( `PV1|1|N|||||||||||||||||^^^^BN|||||||||||||||||||||||||||||||||` )
      ) )
    ).

    " Nachricht transformieren
    TRY.
        transformer->transform(
          EXPORTING
            ir_logger            = me->logger
            ir_message_context   = me->context
            ir_runtime_parameter = me->runtime_params
          CHANGING
            cr_message           = message
        ).
      CATCH cx_ishmed_mci_transform cx_ishmed_mci.
        " Eine Ausnahme beendet die Verarbeitung, das ist eine mögliche
        " korrekte Reaktion
        RETURN.
    ENDTRY.

    " Wenn wir hier angekommen sind, wurde keine Ausnahme ausgelöst. In dem Fall
    " muss eine Fehlernachricht geloggt worden sein UND der Status muss auf
    " 'fehlerhaft' gesetzt sein.
    assert_error_message( ).
    IF me->context->get_status( )
       <> if_ishmed_mci_message_constant=>co_message_status_error.
      cl_abap_unit_assert=>fail(
          msg = 'Transformer has failed to set process status to "error".'
      ).
    ENDIF.

  ENDMETHOD.


  METHOD basic_valid_message.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|1234|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `SCH||APT11223344^ExtSchedAppt|||||ROUTINE^Routine-Termin^HL70276|Complete^Abgeschlossener Termin^HL70277||||||||0246813579^Meijer^Anna||||0246813579^Meijer^Anna|||||Complete^abgeschlossen^HL70278` )
            ( `TQ1|1||||||20180118153000|20180118160000|R^Routine^HL70485` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `PV1|1|O|||||1357924680^Müller^Carina^^^^Dr. med||||||||||||33442211^^^^VN|||||||||||||||||||||||||20180118153000|||||||V` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
            ( `AIL|1|U|IM1AMB^IM1AMB03^^IM1^^^^^Kardiologische Ambulanz 3 (Schrittmacherkontrollraum)|||20180118153000|||90|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
      ) )
    ).

    " Nachricht transformieren und Ergebnisdatentyp prüfen
    transformer->transform(
      EXPORTING
        ir_logger            = me->logger
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
      CHANGING
        cr_message           = message
    ).
    DATA(visit_message) = cast_result_message( message ).

    " Es darf kein Fehler aufgetreten sein:
    assert_no_error_message( ).

    " Schlüsseldaten prüfen
    cl_abap_unit_assert=>assert_equals(
        act = visit_message->get_institution( )
        exp = '1234'
        msg = 'Wrong message contents: INSTITUTION'
    ).
    cl_abap_unit_assert=>assert_equals(
        act = visit_message->get_case_id( )
        exp = '0033442211'
        msg = 'Wrong message contents: PATCASEID'
    ).
    cl_abap_unit_assert=>assert_equals(
        act = me->context->get_external_key( )
        exp = 'APT11223344'
        msg = 'Wrong message context contents: EXTERNAL_KEY'
    ).
    cl_abap_unit_assert=>assert_equals(
        act = visit_message->is_cancelled( )
        exp = abap_false
        msg = 'Wrong message contents: CANCEL_FLAG'
    ).

    " Solldaten für die Bewegungsdaten aufbauen und prüfen
    DATA(expected_visit_data) = VALUE bapi2097ovin(
      movemnt_type    = 'X1'          " Bewegungsart
      movemnt_date    = '20180118'    " Datum der Bewegung
      movemnt_time    = '153000'      " Uhrzeit der Bewegung
      ext_visit_stat  = 'X2'          " Externer Status eines ambulanten Besuchs
      movemnt_enddate = '20180118'    " Bewegungsendedatum
      movemnt_endtime = '160000'      " Bewegungsendezeit
      department      = 'IM1'         " fachliche OE
      nurs_treat_ou   = 'IM1AMB'      " pflegerische OE
      room            = 'IM1AMB03'    " BauId eines Zimmers
      ext_movement_id = 'APT11223344' " Externe Bewegungsidentifikation
      att_phys        = '1357924680'  " Personalnummer des behandelnden Arztes
    ).
    cl_abap_unit_assert=>assert_equals(
        act = visit_message->get_visit_data_adm( )
        exp = expected_visit_data
        msg = 'Wrong message contents: OUTPAT_VISIT_DATA'
    ).

    " medizinische Daten und Landesversion .at sollten leer sein
    cl_abap_unit_assert=>assert_initial(
        act = visit_message->get_visit_data_med( )
        msg = 'Non-initial message contents: OUTPAT_VISIT_DATA_MED'
    ).
    cl_abap_unit_assert=>assert_initial(
        act = visit_message->get_visit_data_med( )
        msg = 'Non-initial message contents: OUTPAT_VISIT_DATA_AT'
    ).

  ENDMETHOD.



  METHOD missing_segment_sch.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|1234|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `TQ1|1||||||20180118153000|20180118160000|R^Routine^HL70485` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `PV1|1|O|||||1357924680^Müller^Carina^^^^Dr. med||||||||||||33442211^^^^VN|||||||||||||||||||||||||20180118153000|||||||V` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
            ( `AIL|1|U|IM1AMB^IM1AMB03^^IM1^^^^^Kardiologische Ambulanz 3 (Schrittmacherkontrollraum)|||20180118153000|||90|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
      ) )
    ).

    " Nachricht transformieren
    TRY.
        transformer->transform(
          EXPORTING
            ir_logger            = me->logger
            ir_message_context   = me->context
            ir_runtime_parameter = me->runtime_params
          CHANGING
            cr_message           = message
        ).
      CATCH cx_ishmed_mci_transform cx_ishmed_mci.
        " Eine Ausnahme beendet die Verarbeitung, das ist eine mögliche
        " korrekte Reaktion
        RETURN.
    ENDTRY.

    " Wenn wir hier angekommen sind, wurde keine Ausnahme ausgelöst. In dem Fall
    " muss eine Fehlernachricht geloggt worden sein UND der Status muss auf
    " 'fehlerhaft' gesetzt sein.
    assert_error_message( ).
    IF me->context->get_status( )
       <> if_ishmed_mci_message_constant=>co_message_status_error.
      cl_abap_unit_assert=>fail(
          msg    = 'Transformer has failed to set process status to "error".'
      ).
    ENDIF.

  ENDMETHOD.

  METHOD missing_segment_tq1.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|1234|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `SCH||APT11223344^ExtSchedAppt|||||ROUTINE^Routine-Termin^HL70276|Complete^Abgeschlossener Termin^HL70277||||||||0246813579^Meijer^Anna||||0246813579^Meijer^Anna|||||Complete^abgeschlossen^HL70278` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `PV1|1|O|||||1357924680^Müller^Carina^^^^Dr. med||||||||||||33442211^^^^VN|||||||||||||||||||||||||20180118153000|||||||V` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
            ( `AIL|1|U|IM1AMB^IM1AMB03^^IM1^^^^^Kardiologische Ambulanz 3 (Schrittmacherkontrollraum)|||20180118153000|||90|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
      ) )
    ).

    " Nachricht transformieren
    TRY.
        transformer->transform(
          EXPORTING
            ir_logger            = me->logger
            ir_message_context   = me->context
            ir_runtime_parameter = me->runtime_params
          CHANGING
            cr_message           = message
        ).
      CATCH cx_ishmed_mci_transform cx_ishmed_mci.
        " Eine Ausnahme beendet die Verarbeitung, das ist eine mögliche
        " korrekte Reaktion
        RETURN.
    ENDTRY.

    " Wenn wir hier angekommen sind, wurde keine Ausnahme ausgelöst. In dem Fall
    " muss eine Fehlernachricht geloggt worden sein UND der Status muss auf
    " 'fehlerhaft' gesetzt sein.
    assert_error_message( ).
    IF me->context->get_status( )
       <> if_ishmed_mci_message_constant=>co_message_status_error.
      cl_abap_unit_assert=>fail(
          msg    = 'Transformer has failed to set process status to "error".'
      ).
    ENDIF.

  ENDMETHOD.

  METHOD missing_segment_pv1.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|1234|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `SCH||APT11223344^ExtSchedAppt|||||ROUTINE^Routine-Termin^HL70276|Complete^Abgeschlossener Termin^HL70277||||||||0246813579^Meijer^Anna||||0246813579^Meijer^Anna|||||Complete^abgeschlossen^HL70278` )
            ( `TQ1|1||||||20180118153000|20180118160000|R^Routine^HL70485` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
            ( `AIL|1|U|IM1AMB^IM1AMB03^^IM1^^^^^Kardiologische Ambulanz 3 (Schrittmacherkontrollraum)|||20180118153000|||90|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
      ) )
    ).

    " Nachricht transformieren
    TRY.
        transformer->transform(
          EXPORTING
            ir_logger            = me->logger
            ir_message_context   = me->context
            ir_runtime_parameter = me->runtime_params
          CHANGING
            cr_message           = message
        ).
      CATCH cx_ishmed_mci_transform cx_ishmed_mci.
        " Eine Ausnahme beendet die Verarbeitung, das ist eine mögliche
        " korrekte Reaktion
        RETURN.
    ENDTRY.

    " Wenn wir hier angekommen sind, wurde keine Ausnahme ausgelöst. In dem Fall
    " muss eine Fehlernachricht geloggt worden sein UND der Status muss auf
    " 'fehlerhaft' gesetzt sein.
    assert_error_message( ).
    IF me->context->get_status( )
       <> if_ishmed_mci_message_constant=>co_message_status_error.
      cl_abap_unit_assert=>fail(
          msg    = 'Transformer has failed to set process status to "error".'
      ).
    ENDIF.

  ENDMETHOD.

  METHOD missing_segment_ail.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|1234|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `SCH||APT11223344^ExtSchedAppt|||||ROUTINE^Routine-Termin^HL70276|Complete^Abgeschlossener Termin^HL70277||||||||0246813579^Meijer^Anna||||0246813579^Meijer^Anna|||||Complete^abgeschlossen^HL70278` )
            ( `TQ1|1||||||20180118153000|20180118160000|R^Routine^HL70485` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `PV1|1|O|||||1357924680^Müller^Carina^^^^Dr. med||||||||||||33442211^^^^VN|||||||||||||||||||||||||20180118153000|||||||V` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
      ) )
    ).

    " Nachricht transformieren
    TRY.
        transformer->transform(
          EXPORTING
            ir_logger            = me->logger
            ir_message_context   = me->context
            ir_runtime_parameter = me->runtime_params
          CHANGING
            cr_message           = message
        ).
      CATCH cx_ishmed_mci_transform cx_ishmed_mci.
        " Eine Ausnahme beendet die Verarbeitung, das ist eine mögliche
        " korrekte Reaktion
        RETURN.
    ENDTRY.

    " Wenn wir hier angekommen sind, wurde keine Ausnahme ausgelöst. In dem Fall
    " muss eine Fehlernachricht geloggt worden sein UND der Status muss auf
    " 'fehlerhaft' gesetzt sein.
    assert_error_message( ).
    IF me->context->get_status( )
       <> if_ishmed_mci_message_constant=>co_message_status_error.
      cl_abap_unit_assert=>fail(
          msg    = 'Transformer has failed to set process status to "error".'
      ).
    ENDIF.

  ENDMETHOD.

  METHOD alpha_conv_inst_numeric.

    " Hier geben wir eine Einrichtung 1 an, die zu '0001' konvertiert werden muss.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|1|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `SCH||APT11223344^ExtSchedAppt|||||ROUTINE^Routine-Termin^HL70276|Complete^Abgeschlossener Termin^HL70277||||||||0246813579^Meijer^Anna||||0246813579^Meijer^Anna|||||Complete^abgeschlossen^HL70278` )
            ( `TQ1|1||||||20180118153000|20180118160000|R^Routine^HL70485` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `PV1|1|O|||||1357924680^Müller^Carina^^^^Dr. med||||||||||||33442211^^^^VN|||||||||||||||||||||||||20180118153000|||||||V` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
            ( `AIL|1|U|IM1AMB^IM1AMB03^^IM1^^^^^Kardiologische Ambulanz 3 (Schrittmacherkontrollraum)|||20180118153000|||90|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
      ) )
    ).

    " Nachricht transformieren und Ergebnisdatentyp prüfen
    transformer->transform(
      EXPORTING
        ir_logger            = me->logger
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
      CHANGING
        cr_message           = message
    ).
    DATA(visit_message) = cast_result_message( message ).

    " Es darf kein Fehler aufgetreten sein:
    assert_no_error_message( ).

    " Schlüsseldaten prüfen
    cl_abap_unit_assert=>assert_equals(
        act = visit_message->get_institution( )
        exp = '0001'
        msg = 'Wrong message contents: INSTITUTION'
    ).

  ENDMETHOD.

  METHOD alpha_conv_inst_alphanumeric.

    " Die Einrichtung 'A1' muss so bleiben und darf keine führenden Nullen
    " erhalten.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|A1|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `SCH||APT11223344^ExtSchedAppt|||||ROUTINE^Routine-Termin^HL70276|Complete^Abgeschlossener Termin^HL70277||||||||0246813579^Meijer^Anna||||0246813579^Meijer^Anna|||||Complete^abgeschlossen^HL70278` )
            ( `TQ1|1||||||20180118153000|20180118160000|R^Routine^HL70485` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `PV1|1|O|||||1357924680^Müller^Carina^^^^Dr. med||||||||||||33442211^^^^VN|||||||||||||||||||||||||20180118153000|||||||V` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
            ( `AIL|1|U|IM1AMB^IM1AMB03^^IM1^^^^^Kardiologische Ambulanz 3 (Schrittmacherkontrollraum)|||20180118153000|||90|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
      ) )
    ).

    " Nachricht transformieren und Ergebnisdatentyp prüfen
    transformer->transform(
      EXPORTING
        ir_logger            = me->logger
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
      CHANGING
        cr_message           = message
    ).
    DATA(visit_message) = cast_result_message( message ).

    " Es darf kein Fehler aufgetreten sein:
    assert_no_error_message( ).

    " Schlüsseldaten prüfen
    cl_abap_unit_assert=>assert_equals(
        act = visit_message->get_institution( )
        exp = 'A1'
        msg = 'Wrong message contents: INSTITUTION'
    ).

  ENDMETHOD.

  METHOD alpha_conv_case_numeric.

    " Die Fallnummer '12' muss zu '0000000012' ergänzt werden.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|1234|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `SCH||APT11223344^ExtSchedAppt|||||ROUTINE^Routine-Termin^HL70276|Complete^Abgeschlossener Termin^HL70277||||||||0246813579^Meijer^Anna||||0246813579^Meijer^Anna|||||Complete^abgeschlossen^HL70278` )
            ( `TQ1|1||||||20180118153000|20180118160000|R^Routine^HL70485` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `PV1|1|O|||||1357924680^Müller^Carina^^^^Dr. med||||||||||||12^^^^VN|||||||||||||||||||||||||20180118153000|||||||V` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
            ( `AIL|1|U|IM1AMB^IM1AMB03^^IM1^^^^^Kardiologische Ambulanz 3 (Schrittmacherkontrollraum)|||20180118153000|||90|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
      ) )
    ).

    " Nachricht transformieren und Ergebnisdatentyp prüfen
    transformer->transform(
      EXPORTING
        ir_logger            = me->logger
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
      CHANGING
        cr_message           = message
    ).
    DATA(visit_message) = cast_result_message( message ).

    " Es darf kein Fehler aufgetreten sein:
    assert_no_error_message( ).

    " Schlüsseldaten prüfen
    cl_abap_unit_assert=>assert_equals(
        act = visit_message->get_case_id( )
        exp = '0000000012'
        msg = 'Wrong message contents: PATCASEID'
    ).

  ENDMETHOD.

  METHOD verify_fields_start_date_time.

    " HL7 erlaubt unvollständige Zeitstempel. Hier muss '2018012309'
    " zu '23.01.2018' und '09:00:00' umgesetzt werden.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|1234|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `SCH||APT11223344^ExtSchedAppt|||||ROUTINE^Routine-Termin^HL70276|Complete^Abgeschlossener Termin^HL70277||||||||0246813579^Meijer^Anna||||0246813579^Meijer^Anna|||||Complete^abgeschlossen^HL70278` )
            ( `TQ1|1||||||2018012309|20180123160000|R^Routine^HL70485` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `PV1|1|O|||||1357924680^Müller^Carina^^^^Dr. med||||||||||||33442211^^^^VN|||||||||||||||||||||||||20180118153000|||||||V` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
            ( `AIL|1|U|IM1AMB^IM1AMB03^^IM1^^^^^Kardiologische Ambulanz 3 (Schrittmacherkontrollraum)|||20180118153000|||90|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
      ) )
    ).

    " Nachricht transformieren und Ergebnisdatentyp prüfen
    transformer->transform(
      EXPORTING
        ir_logger            = me->logger
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
      CHANGING
        cr_message           = message
    ).
    DATA(visit_message) = cast_result_message( message ).
    assert_no_error_message( ).

    " Datenfeld(er) isoliert prüfen
    DATA(visit_data) = visit_message->get_visit_data_adm( ).
    cl_abap_unit_assert=>assert_equals(
        act = visit_data-movemnt_date
        exp = '20180123'
    ).
    cl_abap_unit_assert=>assert_equals(
        act = visit_data-movemnt_time
        exp = '090000'
    ).

  ENDMETHOD.

  METHOD verify_fields_end_date_time.

    " HL7 erlaubt unvollständige Zeitstempel. Hier muss '2018012322'
    " zu '23.01.2018' und '22:00:00' umgesetzt werden.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|1234|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `SCH||APT11223344^ExtSchedAppt|||||ROUTINE^Routine-Termin^HL70276|Complete^Abgeschlossener Termin^HL70277||||||||0246813579^Meijer^Anna||||0246813579^Meijer^Anna|||||Complete^abgeschlossen^HL70278` )
            ( `TQ1|1||||||20180123153000|2018012322|R^Routine^HL70485` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `PV1|1|O|||||1357924680^Müller^Carina^^^^Dr. med||||||||||||33442211^^^^VN|||||||||||||||||||||||||20180118153000|||||||V` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
            ( `AIL|1|U|IM1AMB^IM1AMB03^^IM1^^^^^Kardiologische Ambulanz 3 (Schrittmacherkontrollraum)|||20180118153000|||90|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
      ) )
    ).

    " Nachricht transformieren und Ergebnisdatentyp prüfen
    transformer->transform(
      EXPORTING
        ir_logger            = me->logger
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
      CHANGING
        cr_message           = message
    ).
    DATA(visit_message) = cast_result_message( message ).
    assert_no_error_message( ).

    " Datenfeld(er) isoliert prüfen
    DATA(visit_data) = visit_message->get_visit_data_adm( ).
    cl_abap_unit_assert=>assert_equals(
        act = visit_data-movemnt_enddate
        exp = '20180123'
    ).
    cl_abap_unit_assert=>assert_equals(
        act = visit_data-movemnt_endtime
        exp = '220000'
    ).

  ENDMETHOD.

  METHOD verify_fields_dept_ou.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|1234|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `SCH||APT11223344^ExtSchedAppt|||||ROUTINE^Routine-Termin^HL70276|Complete^Abgeschlossener Termin^HL70277||||||||0246813579^Meijer^Anna||||0246813579^Meijer^Anna|||||Complete^abgeschlossen^HL70278` )
            ( `TQ1|1||||||20180118153000|20180118160000|R^Routine^HL70485` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `PV1|1|O|||||1357924680^Müller^Carina^^^^Dr. med||||||||||||33442211^^^^VN|||||||||||||||||||||||||20180118153000|||||||V` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
            ( `AIL|1|U|IM1AMB^IM1AMB03^^IM2^^^^^Kardiologische Ambulanz 3 (Schrittmacherkontrollraum)|||20180118153000|||90|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
      ) )
    ).

    " Nachricht transformieren und Ergebnisdatentyp prüfen
    transformer->transform(
      EXPORTING
        ir_logger            = me->logger
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
      CHANGING
        cr_message           = message
    ).
    DATA(visit_message) = cast_result_message( message ).
    assert_no_error_message( ).

    " Datenfeld(er) isoliert prüfen
    DATA(visit_data) = visit_message->get_visit_data_adm( ).
    cl_abap_unit_assert=>assert_equals(
        act = visit_data-department
        exp = 'IM2'
    ).

  ENDMETHOD.

  METHOD verify_fields_nurs_ou.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|1234|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `SCH||APT11223344^ExtSchedAppt|||||ROUTINE^Routine-Termin^HL70276|Complete^Abgeschlossener Termin^HL70277||||||||0246813579^Meijer^Anna||||0246813579^Meijer^Anna|||||Complete^abgeschlossen^HL70278` )
            ( `TQ1|1||||||20180118153000|20180118160000|R^Routine^HL70485` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `PV1|1|O|||||1357924680^Müller^Carina^^^^Dr. med||||||||||||33442211^^^^VN|||||||||||||||||||||||||20180118153000|||||||V` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
            ( `AIL|1|U|IM2AMB^IM1AMB03^^IM1^^^^^Kardiologische Ambulanz 3 (Schrittmacherkontrollraum)|||20180118153000|||90|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
      ) )
    ).

    " Nachricht transformieren und Ergebnisdatentyp prüfen
    transformer->transform(
      EXPORTING
        ir_logger            = me->logger
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
      CHANGING
        cr_message           = message
    ).
    DATA(visit_message) = cast_result_message( message ).
    assert_no_error_message( ).

    " Datenfeld(er) isoliert prüfen
    DATA(visit_data) = visit_message->get_visit_data_adm( ).
    cl_abap_unit_assert=>assert_equals(
        act = visit_data-nurs_treat_ou
        exp = 'IM2AMB'
    ).

  ENDMETHOD.

  METHOD verify_fields_room.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|1234|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `SCH||APT11223344^ExtSchedAppt|||||ROUTINE^Routine-Termin^HL70276|Complete^Abgeschlossener Termin^HL70277||||||||0246813579^Meijer^Anna||||0246813579^Meijer^Anna|||||Complete^abgeschlossen^HL70278` )
            ( `TQ1|1||||||20180118153000|20180118160000|R^Routine^HL70485` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `PV1|1|O|||||1357924680^Müller^Carina^^^^Dr. med||||||||||||33442211^^^^VN|||||||||||||||||||||||||20180118153000|||||||V` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
            ( `AIL|1|U|IM1AMB^IM1AMB42^^IM1^^^^^Kardiologische Ambulanz 3 (Schrittmacherkontrollraum)|||20180118153000|||90|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
      ) )
    ).

    " Nachricht transformieren und Ergebnisdatentyp prüfen
    transformer->transform(
      EXPORTING
        ir_logger            = me->logger
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
      CHANGING
        cr_message           = message
    ).
    DATA(visit_message) = cast_result_message( message ).
    assert_no_error_message( ).

    " Datenfeld(er) isoliert prüfen
    DATA(visit_data) = visit_message->get_visit_data_adm( ).
    cl_abap_unit_assert=>assert_equals(
        act = visit_data-room
        exp = 'IM1AMB42'
    ).

  ENDMETHOD.

  METHOD verify_fields_ext_movement_id.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|1234|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `SCH||FOOBAR42^ExtSchedAppt|||||ROUTINE^Routine-Termin^HL70276|Complete^Abgeschlossener Termin^HL70277||||||||0246813579^Meijer^Anna||||0246813579^Meijer^Anna|||||Complete^abgeschlossen^HL70278` )
            ( `TQ1|1||||||20180118153000|20180118160000|R^Routine^HL70485` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `PV1|1|O|||||1357924680^Müller^Carina^^^^Dr. med||||||||||||33442211^^^^VN|||||||||||||||||||||||||20180118153000|||||||V` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
            ( `AIL|1|U|IM1AMB^IM1AMB03^^IM1^^^^^Kardiologische Ambulanz 3 (Schrittmacherkontrollraum)|||20180118153000|||90|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
      ) )
    ).

    " Nachricht transformieren und Ergebnisdatentyp prüfen
    transformer->transform(
      EXPORTING
        ir_logger            = me->logger
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
      CHANGING
        cr_message           = message
    ).
    DATA(visit_message) = cast_result_message( message ).
    assert_no_error_message( ).

    " Datenfeld(er) isoliert prüfen
    DATA(visit_data) = visit_message->get_visit_data_adm( ).
    cl_abap_unit_assert=>assert_equals(
        act = visit_data-ext_movement_id
        exp = 'FOOBAR42'
    ).

  ENDMETHOD.

  METHOD verify_fields_att_phys.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|1234|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `SCH||APT11223344^ExtSchedAppt|||||ROUTINE^Routine-Termin^HL70276|Complete^Abgeschlossener Termin^HL70277||||||||0246813579^Meijer^Anna||||0246813579^Meijer^Anna|||||Complete^abgeschlossen^HL70278` )
            ( `TQ1|1||||||20180118153000|20180118160000|R^Routine^HL70485` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `PV1|1|O|||||9988776655^Müller^Carina^^^^Dr. med||||||||||||33442211^^^^VN|||||||||||||||||||||||||20180118153000|||||||V` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
            ( `AIL|1|U|IM1AMB^IM1AMB03^^IM1^^^^^Kardiologische Ambulanz 3 (Schrittmacherkontrollraum)|||20180118153000|||90|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
      ) )
    ).

    " Nachricht transformieren und Ergebnisdatentyp prüfen
    transformer->transform(
      EXPORTING
        ir_logger            = me->logger
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
      CHANGING
        cr_message           = message
    ).
    DATA(visit_message) = cast_result_message( message ).
    assert_no_error_message( ).

    " Datenfeld(er) isoliert prüfen
    DATA(visit_data) = visit_message->get_visit_data_adm( ).
    cl_abap_unit_assert=>assert_equals(
        act = visit_data-att_phys
        exp = '9988776655'
    ).

  ENDMETHOD.

  METHOD verify_cancel_flag_cancelled_l.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|1234|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `SCH||APT11223344^ExtSchedAppt|||||ROUTINE^Routine-Termin^HL70276|Complete^Abgeschlossener Termin^HL70277||||||||0246813579^Meijer^Anna||||0246813579^Meijer^Anna|||||cancelled^storniert^HL70278` )
            ( `TQ1|1||||||20180118153000|20180118160000|R^Routine^HL70485` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `PV1|1|O|||||9988776655^Müller^Carina^^^^Dr. med||||||||||||33442211^^^^VN|||||||||||||||||||||||||20180118153000|||||||V` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
            ( `AIL|1|U|IM1AMB^IM1AMB03^^IM1^^^^^Kardiologische Ambulanz 3 (Schrittmacherkontrollraum)|||20180118153000|||90|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
      ) )
    ).

    " Nachricht transformieren und Ergebnisdatentyp prüfen
    transformer->transform(
      EXPORTING
        ir_logger            = me->logger
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
      CHANGING
        cr_message           = message
    ).
    DATA(visit_message) = cast_result_message( message ).
    assert_no_error_message( ).

    " Storno-Kennzeichen isoliert prüfen
    cl_abap_unit_assert=>assert_true(
        act = visit_message->is_cancelled( )
    ).

  ENDMETHOD.

  METHOD verify_cancel_flag_cancelled_m.


    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|1234|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `SCH||APT11223344^ExtSchedAppt|||||ROUTINE^Routine-Termin^HL70276|Complete^Abgeschlossener Termin^HL70277||||||||0246813579^Meijer^Anna||||0246813579^Meijer^Anna|||||Cancelled^storniert^HL70278` )
            ( `TQ1|1||||||20180118153000|20180118160000|R^Routine^HL70485` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `PV1|1|O|||||9988776655^Müller^Carina^^^^Dr. med||||||||||||33442211^^^^VN|||||||||||||||||||||||||20180118153000|||||||V` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
            ( `AIL|1|U|IM1AMB^IM1AMB03^^IM1^^^^^Kardiologische Ambulanz 3 (Schrittmacherkontrollraum)|||20180118153000|||90|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
      ) )
    ).

    " Nachricht transformieren und Ergebnisdatentyp prüfen
    transformer->transform(
      EXPORTING
        ir_logger            = me->logger
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
      CHANGING
        cr_message           = message
    ).
    DATA(visit_message) = cast_result_message( message ).
    assert_no_error_message( ).

    " Storno-Kennzeichen isoliert prüfen
    cl_abap_unit_assert=>assert_true(
        act = visit_message->is_cancelled( )
    ).
  ENDMETHOD.

  METHOD verify_cancel_flag_cancelled_u.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|1234|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `SCH||APT11223344^ExtSchedAppt|||||ROUTINE^Routine-Termin^HL70276|Complete^Abgeschlossener Termin^HL70277||||||||0246813579^Meijer^Anna||||0246813579^Meijer^Anna|||||CANCELLED^storniert^HL70278` )
            ( `TQ1|1||||||20180118153000|20180118160000|R^Routine^HL70485` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `PV1|1|O|||||9988776655^Müller^Carina^^^^Dr. med||||||||||||33442211^^^^VN|||||||||||||||||||||||||20180118153000|||||||V` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
            ( `AIL|1|U|IM1AMB^IM1AMB03^^IM1^^^^^Kardiologische Ambulanz 3 (Schrittmacherkontrollraum)|||20180118153000|||90|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
      ) )
    ).

    " Nachricht transformieren und Ergebnisdatentyp prüfen
    transformer->transform(
      EXPORTING
        ir_logger            = me->logger
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
      CHANGING
        cr_message           = message
    ).
    DATA(visit_message) = cast_result_message( message ).
    assert_no_error_message( ).

    " Storno-Kennzeichen isoliert prüfen
    cl_abap_unit_assert=>assert_true(
        act = visit_message->is_cancelled( )
    ).
  ENDMETHOD.

  METHOD verify_cancel_flag_deleted_l.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|1234|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `SCH||APT11223344^ExtSchedAppt|||||ROUTINE^Routine-Termin^HL70276|Complete^Abgeschlossener Termin^HL70277||||||||0246813579^Meijer^Anna||||0246813579^Meijer^Anna|||||deleted^gelöscht^HL70278` )
            ( `TQ1|1||||||20180118153000|20180118160000|R^Routine^HL70485` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `PV1|1|O|||||9988776655^Müller^Carina^^^^Dr. med||||||||||||33442211^^^^VN|||||||||||||||||||||||||20180118153000|||||||V` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
            ( `AIL|1|U|IM1AMB^IM1AMB03^^IM1^^^^^Kardiologische Ambulanz 3 (Schrittmacherkontrollraum)|||20180118153000|||90|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
      ) )
    ).

    " Nachricht transformieren und Ergebnisdatentyp prüfen
    transformer->transform(
      EXPORTING
        ir_logger            = me->logger
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
      CHANGING
        cr_message           = message
    ).
    DATA(visit_message) = cast_result_message( message ).
    assert_no_error_message( ).

    " Storno-Kennzeichen isoliert prüfen
    cl_abap_unit_assert=>assert_true(
        act = visit_message->is_cancelled( )
    ).

  ENDMETHOD.

  METHOD verify_cancel_flag_deleted_m.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|1234|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `SCH||APT11223344^ExtSchedAppt|||||ROUTINE^Routine-Termin^HL70276|Complete^Abgeschlossener Termin^HL70277||||||||0246813579^Meijer^Anna||||0246813579^Meijer^Anna|||||Deleted^gelöscht^HL70278` )
            ( `TQ1|1||||||20180118153000|20180118160000|R^Routine^HL70485` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `PV1|1|O|||||9988776655^Müller^Carina^^^^Dr. med||||||||||||33442211^^^^VN|||||||||||||||||||||||||20180118153000|||||||V` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
            ( `AIL|1|U|IM1AMB^IM1AMB03^^IM1^^^^^Kardiologische Ambulanz 3 (Schrittmacherkontrollraum)|||20180118153000|||90|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
      ) )
    ).

    " Nachricht transformieren und Ergebnisdatentyp prüfen
    transformer->transform(
      EXPORTING
        ir_logger            = me->logger
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
      CHANGING
        cr_message           = message
    ).
    DATA(visit_message) = cast_result_message( message ).
    assert_no_error_message( ).

    " Storno-Kennzeichen isoliert prüfen
    cl_abap_unit_assert=>assert_true(
        act = visit_message->is_cancelled( )
    ).

  ENDMETHOD.

  METHOD verify_cancel_flag_deleted_u.

    " HL7-Testnachricht aus Text erzeugen
    DATA(message) = CAST if_ishmed_mci_message(
      create_hl7_message_from_text(
          i_message = VALUE #(
            ( `MSH|^~\&|ExtSched||i.s.h.med|1234|20180118191432||SIU^S12^SIU_S12|MSG-20180118-191432-0525|P|2.6|||NE|NE||UNICODE` )
            ( `SCH||APT11223344^ExtSchedAppt|||||ROUTINE^Routine-Termin^HL70276|Complete^Abgeschlossener Termin^HL70277||||||||0246813579^Meijer^Anna||||0246813579^Meijer^Anna|||||DELETED^gelöscht^HL70278` )
            ( `TQ1|1||||||20180118153000|20180118160000|R^Routine^HL70485` )
            ( `PID|1||88776655^^^^PI||Testmann^Theodor||19560304|M|||&Musterstraße&12b^^Musterstadt^^12345^^H` )
            ( `PV1|1|O|||||9988776655^Müller^Carina^^^^Dr. med||||||||||||33442211^^^^VN|||||||||||||||||||||||||20180118153000|||||||V` )
            ( `RGS|1|U` )
            ( `AIS|1|U|QQSMKRX^Schrittmacherkontrolle^L|20180118153000|||30|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
            ( `NTE|1|L|NBG SSIR0||1357924680^Müller^Carina^^^^Dr. med.` )
            ( `AIL|1|U|IM1AMB^IM1AMB03^^IM1^^^^^Kardiologische Ambulanz 3 (Schrittmacherkontrollraum)|||20180118153000|||90|min^Minuten^ISO31||Complete^abgeschlossen^HL70278` )
      ) )
    ).

    " Nachricht transformieren und Ergebnisdatentyp prüfen
    transformer->transform(
      EXPORTING
        ir_logger            = me->logger
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
      CHANGING
        cr_message           = message
    ).
    DATA(visit_message) = cast_result_message( message ).
    assert_no_error_message( ).

    " Storno-Kennzeichen isoliert prüfen
    cl_abap_unit_assert=>assert_true(
        act = visit_message->is_cancelled( )
    ).

  ENDMETHOD.

  METHOD multiple_messages_processing.
    basic_valid_message( ).
    me->errorhandler->initialize( ).
    wrong_incoming_type_handling( ).
    me->errorhandler->initialize( ).
    wrong_incoming_hl7_handling( ).
    me->errorhandler->initialize( ).
    basic_valid_message( ).
    me->errorhandler->initialize( ).
    missing_segment_sch( ).
    me->errorhandler->initialize( ).
    missing_segment_tq1( ).
    me->errorhandler->initialize( ).
    missing_segment_pv1( ).
    me->errorhandler->initialize( ).
    missing_segment_ail( ).
    me->errorhandler->initialize( ).
    basic_valid_message( ).
    me->errorhandler->initialize( ).
    alpha_conv_inst_numeric( ).
    me->errorhandler->initialize( ).
    alpha_conv_inst_alphanumeric( ).
    me->errorhandler->initialize( ).
    alpha_conv_case_numeric( ).
    me->errorhandler->initialize( ).
    basic_valid_message( ).
    me->errorhandler->initialize( ).
    verify_fields_start_date_time( ).
    me->errorhandler->initialize( ).
    verify_fields_end_date_time( ).
    me->errorhandler->initialize( ).
    me->errorhandler->initialize( ).
    verify_fields_dept_ou( ).
    me->errorhandler->initialize( ).
    verify_fields_nurs_ou( ).
    me->errorhandler->initialize( ).
    verify_fields_room( ).
    me->errorhandler->initialize( ).
    verify_fields_ext_movement_id( ).
    me->errorhandler->initialize( ).
    verify_fields_att_phys( ).
    me->errorhandler->initialize( ).
    basic_valid_message( ).
  ENDMETHOD.

ENDCLASS.
