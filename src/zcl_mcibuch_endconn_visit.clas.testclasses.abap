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
** Local Subclass with adaptations for test
**
CLASS lcl_connector DEFINITION FOR TESTING
  INHERITING FROM zcl_mcibuch_endconn_visit
  FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    DATA elink_double TYPE REF TO zif_mcibuch_elink_api.
    DATA history_double TYPE REF TO zif_mcibuch_history_api.
    DATA bapi_double TYPE REF TO zif_mcibuch_visit_bapi_wrapper.

    METHODS constructor.

    ALIASES init FOR if_ishmed_mci_component~init.
    ALIASES send FOR if_ishmed_mci_end_connector~send.
    ALIASES finalize FOR if_ishmed_object~finalize.

  PROTECTED SECTION.
    METHODS create_elink_api REDEFINITION.
    METHODS create_history_api REDEFINITION.

ENDCLASS.

CLASS lcl_connector IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    me->bapi_double = CAST #(
      cl_abap_testdouble=>create(
          object_name = 'ZIF_MCIBUCH_VISIT_BAPI_WRAPPER'
          double_name = 'BAPI_WRAPPER'
      )
    ).
    me->bapi_wrapper = me->bapi_double.
    me->elink_double = CAST #(
      cl_abap_testdouble=>create(
          object_name = 'ZIF_MCIBUCH_ELINK_API'
          double_name = 'ELINK_API'
      )
    ).
    me->history_double = CAST #(
      cl_abap_testdouble=>create(
          object_name = 'ZIF_MCIBUCH_HISTORY_API'
          double_name = 'HISTORY_API'
      )
    ).
  ENDMETHOD.

  METHOD create_elink_api.
    r_result = me->elink_double.
  ENDMETHOD.

  METHOD create_history_api.
    r_result = me->history_double.
  ENDMETHOD.

ENDCLASS.

**
** Base Class for Unit Tests
**
CLASS lcl_unit_test_base DEFINITION ABSTRACT
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PUBLIC SECTION.
  PROTECTED SECTION.

    CONSTANTS co_institution_id TYPE einri VALUE '1234'.
    CONSTANTS co_case_id TYPE falnr VALUE '9876543210'.
    CONSTANTS co_movement_type_1 TYPE ri_bwart VALUE 'XY'.
    CONSTANTS co_movement_date_1 TYPE bwidt VALUE '20180101'.
    CONSTANTS co_movement_time_1 TYPE bwizt VALUE '123456'.
    CONSTANTS co_department_1 TYPE nzuwfa VALUE 'DEPT_1'.
    CONSTANTS co_ward_1 TYPE nzuwpf VALUE 'WARD_1'.
    CONSTANTS co_ward_2 TYPE nzuwpf VALUE 'WARD_2'.
    CONSTANTS co_transport_type_1 TYPE n1tpae VALUE 'AB'.
    CONSTANTS co_transport_type_2 TYPE n1tpae VALUE 'CD'.
    CONSTANTS co_external_key TYPE n1mci_external_key VALUE '12EXT34KEY56'.
    CONSTANTS co_movement_number_1 TYPE lfdbew VALUE '00001'.
    CONSTANTS co_movement_number_2 TYPE lfdbew VALUE '00002'.
    CONSTANTS co_med_emergency_case_1 TYPE nwatnotf VALUE abap_true.
    CONSTANTS co_med_emergency_case_2 TYPE nwatnotf VALUE abap_false.

    CONSTANTS co_check_message_type TYPE symsgty VALUE 'I'.
    CONSTANTS co_check_message_id TYPE symsgid VALUE '$$TESTMSGID$$'.
    CONSTANTS co_check_message_number_1 TYPE symsgno VALUE '042'.
    CONSTANTS co_check_message_number_2 TYPE symsgno VALUE '142'.
    CONSTANTS co_check_message_number_3 TYPE symsgno VALUE '242'.

    DATA errorhandler TYPE REF TO cl_ishmed_errorhandling.
    DATA logger TYPE REF TO if_ishmed_mci_logger.
    DATA context TYPE REF TO if_ishmed_mci_message_context.
    DATA runtime_params TYPE REF TO if_ishmed_mci_runtime_params.
    DATA connector TYPE REF TO lcl_connector.

    METHODS assert_error_message.
    METHODS assert_warning_message.
    METHODS assert_no_error_message.
    METHODS assert_test_message_logged
      IMPORTING
        i_message_type   TYPE symsgty DEFAULT co_check_message_type
        i_message_id     TYPE symsgid DEFAULT co_check_message_id
        i_message_number TYPE symsgno.

    METHODS assert_process_status
      IMPORTING
        i_status TYPE n1mci_message_status.

    METHODS configure_bapi_call
      RETURNING
        VALUE(r_configuration) TYPE REF TO if_abap_testdouble_config.
    METHODS configure_history_call
      RETURNING
        VALUE(r_configuration) TYPE REF TO if_abap_testdouble_config.
    METHODS configure_elink_call
      RETURNING
        VALUE(r_configuration) TYPE REF TO if_abap_testdouble_config.

    METHODS expect_transaction_handling
      IMPORTING
        i_commit   TYPE abap_bool
        i_rollback TYPE abap_bool.


    METHODS configure_history_response
      IMPORTING
        i_message_date TYPE d OPTIONAL
        i_message_time TYPE t OPTIONAL.
    METHODS expect_history_updated.
    METHODS expect_history_unchanged.
    METHODS expect_history_finalized
      RAISING
        cx_ishmed_object.

    METHODS configure_elink_response
      IMPORTING
        i_objects TYPE rn1tpi_object_id_t OPTIONAL.
    METHODS expect_elink_unqueried.
    METHODS expect_elink_updated
      IMPORTING
        i_object_id TYPE swo_typeid.
    METHODS expect_elink_unchanged.
    METHODS expect_elink_finalized
      RAISING
        cx_ishmed_object.

    METHODS expect_no_getoutpatvisit.
    METHODS expect_no_addoutpatvisit.
    METHODS expect_no_changeoutpatvisit.
    METHODS expect_no_canceloutpatvisit.

    METHODS create_default_message
      RETURNING
        VALUE(r_message) TYPE REF TO zcl_mcibuch_msg_case_visit.

  PRIVATE SECTION.
    METHODS setup
      RAISING
        cx_ishmed_mci.
    METHODS teardown
      RAISING
        cx_ishmed_object.

ENDCLASS.

CLASS lcl_unit_test_base IMPLEMENTATION.

  METHOD setup.
    " Logging initialisieren
    me->errorhandler = NEW cl_ishmed_errorhandling( ).
    me->logger = NEW cl_ishmed_mci_log_errorhandler(
        ir_errorhandler = me->errorhandler
    ).

    " Kontext mit den Standardwerten versorgen
    me->context = NEW lcl_context( ).
    me->context->set_einri( co_institution_id ).
    me->context->set_falnr( co_case_id ).
    me->context->set_message_date( sy-datum ).
    me->context->set_message_time( sy-uzeit ).
    me->context->set_external_key( co_external_key ).

    " Laufzeitparameter bereitstellen
    me->runtime_params = NEW lcl_runtime_params(
        i_process_id   = '00042000DEADBEEF001234'
        i_process_name = 'ZZ_UNIT_TEST_PROCESS'
    ).

    " zu testenden Konnektor initialisieren
    me->connector = NEW lcl_connector( ).
    me->connector->init(
        i_type             = if_ishmed_mci_comp_constant=>co_end_connector
        ir_logger          = me->logger
    ).
  ENDMETHOD.

  METHOD teardown.
    me->connector->finalize( ).
    FREE me->connector.
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
      cl_abap_unit_assert=>fail(
          msg  = 'Expected error message not logged'
          quit = if_aunit_constants=>method
      ).
    ENDIF.
  ENDMETHOD.

  METHOD assert_warning_message.
    me->errorhandler->get_max_errortype(
      IMPORTING
        e_maxty         = DATA(worst_message_type)
    ).
    IF worst_message_type NA 'W'.
      cl_abap_unit_assert=>fail(
          msg  = 'Expected warning message not logged'
          quit = if_aunit_constants=>method
      ).
    ENDIF.
  ENDMETHOD.

  METHOD assert_no_error_message.
    me->errorhandler->get_max_errortype(
      IMPORTING
        e_maxty         = DATA(worst_message_type)
    ).
    IF worst_message_type CA 'EAX'.
      cl_abap_unit_assert=>fail(
          msg  = 'Unexpected error message logged'
          quit = if_aunit_constants=>method
      ).
    ENDIF.
  ENDMETHOD.

  METHOD assert_test_message_logged.
    me->errorhandler->get_messages(
      IMPORTING
        t_messages      = DATA(messages)
    ).
    READ TABLE messages
      WITH KEY type   = i_message_type
               id     = i_message_id
               number = i_message_number
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail(
          msg  = |Expected message x{ i_message_number }({ i_message_id }) not logged|
          quit = if_aunit_constants=>method
      ).
    ENDIF.
  ENDMETHOD.

  METHOD assert_process_status.
    cl_abap_unit_assert=>assert_equals(
        act = me->context->get_status( )
        exp = i_status
        msg = 'Unexpected message status'
    ).
  ENDMETHOD.

  METHOD configure_bapi_call.
    r_configuration = cl_abap_testdouble=>configure_call(
        me->connector->bapi_double
    ).
  ENDMETHOD.

  METHOD configure_history_call.
    r_configuration = cl_abap_testdouble=>configure_call(
        me->connector->history_double
    ).
  ENDMETHOD.

  METHOD configure_elink_call.
    r_configuration = cl_abap_testdouble=>configure_call(
        me->connector->elink_double
    ).
  ENDMETHOD.

  METHOD expect_transaction_handling.

    IF i_commit = abap_true.
      configure_bapi_call( )->ignore_all_parameters( )->and_expect( )->is_called_once( ).
    ELSE.
      configure_bapi_call( )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    ENDIF.
    me->connector->bapi_double->call_transaction_commit( ).

    IF i_rollback = abap_true.
      configure_bapi_call( )->ignore_all_parameters( )->and_expect( )->is_called_once( ).
    ELSE.
      configure_bapi_call( )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    ENDIF.
    me->connector->bapi_double->call_transaction_rollback( ).

  ENDMETHOD.

  METHOD create_default_message.

    r_message  = NEW zcl_mcibuch_msg_case_visit( ).
    r_message->set_institution( co_institution_id ).
    r_message->set_case_id( co_case_id ).
    r_message->set_cancelled( abap_false ).
    r_message->set_visit_data_adm( VALUE #(
      movemnt_type       = co_movement_type_1
      movemnt_date       = co_movement_date_1
      movemnt_time       = co_movement_time_1
      department         = co_department_1
      nurs_treat_ou      = co_ward_1
    ) ).
    r_message->set_visit_data_med( VALUE #(
      transport_type     = co_transport_type_1
    ) ).
    r_message->set_visit_data_at( VALUE #(
      med_emergency_case = co_med_emergency_case_1
    ) ).

  ENDMETHOD.

  METHOD configure_history_response.
    configure_history_call( )->returning( value = i_message_date ).
    me->connector->history_double->get_message_date( ).

    configure_history_call( )->returning( value = i_message_time ).
    me->connector->history_double->get_message_time( ).
  ENDMETHOD.

  METHOD expect_history_updated.

    configure_history_call( )->and_expect( )->is_called_once( ).
    me->connector->history_double->set_message_date( sy-datum ).

    configure_history_call( )->and_expect( )->is_called_once( ).
    me->connector->history_double->set_message_time( sy-uzeit ).

    configure_history_call( )->and_expect( )->is_called_once( ).
    me->connector->history_double->save( ).

  ENDMETHOD.


  METHOD expect_history_unchanged.

    configure_history_call( )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    me->connector->history_double->set_message_date( sy-datum ).

    configure_history_call( )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    me->connector->history_double->set_message_time( sy-uzeit ).

    configure_history_call( )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    me->connector->history_double->save( ).

  ENDMETHOD.

  METHOD expect_history_finalized.
    configure_history_call( )->and_expect( )->is_called_once( ).
    me->connector->history_double->if_ishmed_object~finalize( ).
  ENDMETHOD.

  METHOD configure_elink_response.
    configure_elink_call( )->returning(
        value = i_objects
    ).
    me->connector->elink_double->get_object_id_t( ).
  ENDMETHOD.

  METHOD expect_elink_unqueried.
    configure_elink_call( )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    me->connector->elink_double->get_object_id_t( ).
  ENDMETHOD.

  METHOD expect_elink_updated.
    configure_elink_call( )->and_expect( )->is_called_once( ).
    me->connector->elink_double->add( i_object_id ).
    configure_elink_call( )->and_expect( )->is_called_once( ).
    me->connector->elink_double->save( ).
  ENDMETHOD.

  METHOD expect_elink_unchanged.
    configure_elink_call( )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    me->connector->elink_double->add( VALUE #( ) ).

    configure_elink_call( )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    me->connector->elink_double->save( ).
  ENDMETHOD.

  METHOD expect_elink_finalized.
    configure_elink_call( )->and_expect( )->is_called_once( ).
    me->connector->elink_double->if_ishmed_object~finalize( ).
  ENDMETHOD.

  METHOD expect_no_getoutpatvisit.
    configure_bapi_call( )->and_expect( )->is_never_called( ).
    me->connector->bapi_double->call_patcase_getoutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
    ).
  ENDMETHOD.

  METHOD expect_no_addoutpatvisit.
    configure_bapi_call( )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    me->connector->bapi_double->call_patcase_addoutpatvisit(
      EXPORTING
        i_institution            = co_institution_id
        i_case_id                = co_case_id
        is_outpat_visit_data     = VALUE #( )
        is_outpat_visit_data_med = VALUE #( )
        is_outpat_visit_data_at  = VALUE #( )
        i_testrun                = abap_false
    ).
  ENDMETHOD.

  METHOD expect_no_changeoutpatvisit.
    configure_bapi_call( )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    me->connector->bapi_double->call_patcase_changeoutpatvisit(
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
        is_changed_visit_data  = VALUE #( )
        i_testrun              = abap_false
    ).

  ENDMETHOD.

  METHOD expect_no_canceloutpatvisit.
    configure_bapi_call( )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    me->connector->bapi_double->call_patcase_canceloutpatvisit(
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
        i_testrun              = abap_false
    ).
  ENDMETHOD.

ENDCLASS.


**
** Unit Tests for consistency checks and global error handling
**

CLASS lcl_test_error_handling DEFINITION FINAL
  INHERITING FROM lcl_unit_test_base
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PROTECTED SECTION.

    "! Dieser Test stellt sicher, dass der Transformer bei einer eingehenden
    "! Nachricht eines falschen Typs die Verarbeitung mit einer Ausnahme abbricht
    "! oder einen Fehlerstatus setzt.
    METHODS wrong_incoming_type_handling FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test stellt sicher, dass der Transformer bei unvollständigen
    "! Kontextdaten (hier: Einrichtung) kontrolliert abbricht.
    METHODS incomplete_context_institution FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test stellt sicher, dass der Transformer bei unvollständigen
    "! Kontextdaten (hier: Fallnummer) kontrolliert abbricht.
    METHODS incomplete_context_case FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test stellt sicher, dass der Transformer bei unvollständigen
    "! Kontextdaten (hier: externer Schlüssel) kontrolliert abbricht.
    METHODS incomplete_context_ext_key FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Dieser Test stellt sicher, dass der Transformer bei unvollständigen
    "! Kontextdaten (hier: Nachrichtendatum) kontrolliert abbricht.
    "! Hinweis: Zeit '000000' ist gültig und wird deshalb nicht geprüft.
    METHODS incomplete_context_msg_date FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

ENDCLASS.

CLASS lcl_test_error_handling IMPLEMENTATION.

  METHOD wrong_incoming_type_handling.

    " Der End-Konnektor benötigt eine Besuchs-Nachricht. Zum Test der Fehler-
    " behandlung versorgen wir ihn mit einer Nachricht eines ganz anderen Typs.
    DATA(invalid_message) = CAST if_ishmed_mci_message(
      NEW cl_ishmed_mci_string_message( )
    ).

    " Nachricht verarbeiten
    TRY.
        connector->send(
            ir_logger            = me->logger
            ir_message           = invalid_message
            ir_message_context   = me->context
            ir_runtime_parameter = me->runtime_params
        ).
      CATCH cx_ishmed_mci_send cx_ishmed_mci.
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
          msg = 'Connector has failed to set process status to "error".'
      ).
    ENDIF.

  ENDMETHOD.

  METHOD incomplete_context_institution.

    " Eingabenachricht erzeugen
    DATA(message) = create_default_message( ).

    " Einrichtung leeren
    me->context->set_einri( VALUE #( ) ).
    message->set_institution( VALUE #( ) ).

    " Nachricht verarbeiten
    TRY.
        connector->send(
            ir_logger            = me->logger
            ir_message           = message
            ir_message_context   = me->context
            ir_runtime_parameter = me->runtime_params
        ).
      CATCH cx_ishmed_mci.
        " Ausnahme führt zu einem Prozessabbruch und ist damit OK.
        RETURN.
    ENDTRY.

    " es muss zu einem Fehler gekommen sein
    assert_error_message( ).
    cl_abap_unit_assert=>assert_equals(
        act = me->context->get_status( )
        exp = if_ishmed_mci_message_constant=>co_message_status_error
        msg = 'Unexpected message status'
    ).

  ENDMETHOD.

  METHOD incomplete_context_case.

    " Eingabenachricht erzeugen
    DATA(message) = create_default_message( ).

    " Fall leeren
    me->context->set_falnr( VALUE #( ) ).
    message->set_case_id( VALUE #( ) ).

    " Nachricht verarbeiten
    TRY.
        connector->send(
            ir_logger            = me->logger
            ir_message           = message
            ir_message_context   = me->context
            ir_runtime_parameter = me->runtime_params
        ).
      CATCH cx_ishmed_mci.
        " Ausnahme führt zu einem Prozessabbruch und ist damit OK.
        RETURN.
    ENDTRY.

    " es muss zu einem Fehler gekommen sein
    assert_error_message( ).
    cl_abap_unit_assert=>assert_equals(
        act = me->context->get_status( )
        exp = if_ishmed_mci_message_constant=>co_message_status_error
        msg = 'Unexpected message status'
    ).

  ENDMETHOD.

  METHOD incomplete_context_ext_key.

    " Eingabenachricht erzeugen
    DATA(message) = create_default_message( ).

    " externen Schlüssel leeren
    me->context->set_external_key( VALUE #( ) ).

    " Nachricht verarbeiten
    TRY.
        connector->send(
            ir_logger            = me->logger
            ir_message           = message
            ir_message_context   = me->context
            ir_runtime_parameter = me->runtime_params
        ).
      CATCH cx_ishmed_mci.
        " Ausnahme führt zu einem Prozessabbruch und ist damit OK.
        RETURN.
    ENDTRY.

    " es muss zu einem Fehler gekommen sein
    assert_error_message( ).
    cl_abap_unit_assert=>assert_equals(
        act = me->context->get_status( )
        exp = if_ishmed_mci_message_constant=>co_message_status_error
        msg = 'Unexpected message status'
    ).

  ENDMETHOD.

  METHOD incomplete_context_msg_date.

    " Eingabenachricht erzeugen
    DATA(message) = create_default_message( ).

    " Einrichtung leeren
    me->context->set_message_date( VALUE #( ) ).

    " Nachricht verarbeiten
    TRY.
        connector->send(
            ir_logger            = me->logger
            ir_message           = message
            ir_message_context   = me->context
            ir_runtime_parameter = me->runtime_params
        ).
      CATCH cx_ishmed_mci.
        " Ausnahme führt zu einem Prozessabbruch und ist damit OK.
        RETURN.
    ENDTRY.

    " es muss zu einem Fehler gekommen sein
    assert_error_message( ).
    cl_abap_unit_assert=>assert_equals(
        act = me->context->get_status( )
        exp = if_ishmed_mci_message_constant=>co_message_status_error
        msg = 'Unexpected message status'
    ).

  ENDMETHOD.

ENDCLASS.

**
** Unit Tests for movement creation
**
CLASS lcl_test_create DEFINITION FINAL
  INHERITING FROM lcl_unit_test_base
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PROTECTED SECTION.

    "! Test einer "normalen" Bewegungsanlage.
    METHODS create_normal FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Test der Anlage einer stornierten Bewegung.
    METHODS create_cancelled FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Test einer veralteten Nachricht zur Bewegungsanlage (Datum in Vergangenheit).
    METHODS create_obsolete_date FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Test einer veralteten Nachricht zur Bewegungsanlage (Datum gleich, aber Zeit in Vergangenheit).
    METHODS create_obsolete_time FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Aktualisierung einer stornierten Bewegung führt zu einer Neuanlage
    METHODS create_update_cancelled FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Test einer Bewegungsanlage mit Fehlermeldung im Testmodus.
    METHODS error_test FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Test einer Bewegungsanlage mit Fehlermeldung im Verbuchungsmodus
    METHODS error_update FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

ENDCLASS.

CLASS lcl_test_create IMPLEMENTATION.

  METHOD create_normal.

    " Eingabenachricht erzeugen
    DATA(message) = create_default_message( ).

    " Wir erwarten Anfragen bei der History-API, antworten aber mit Initialwerten
    " (weil wir keine vorherige Nachricht annehmen).
    configure_history_response(
        i_message_date = VALUE d( )
        i_message_time = VALUE t( )
    ).

    " Im Rahmen der Verbuchung muss die History-Tabelle aktualisiert werden.
    expect_history_updated( ).

    " Bei einer Neuanlage gibt es noch kein verknüpftes Objekt.
    configure_elink_response( ).

    " Nach der Anlage muss die Verknüpfungstabelle aktualisiert werden.
    expect_elink_updated(
        i_object_id = CONV #( |{ co_institution_id }{ co_case_id }{ co_movement_number_1 }| )
    ).

    " Die Sperren der History- und Verknüpfungstabelle müssen aufgehoben werden.
    expect_history_finalized( ).
    expect_elink_finalized( ).

    " Da es keine verknüpften Bewegungen gibt, sollte nicht danach gesucht werden.
    expect_no_getoutpatvisit( ).

    " Es sollte eine Bewegung angelegt werden, und zwar zuerst einmal im Testmodus,
    " dann mit Verbuchung.
    configure_bapi_call( )->set_parameter(
        name          = 'ES_NEW_MOVEMENT_DATA'
        value         = VALUE bapi2097move(
          " movemnt_seqno    --> wird im Testmodus nicht vergeben
          movemnt_type       = co_movement_type_1
          movemnt_date       = co_movement_date_1
          movemnt_time       = co_movement_time_1
          department         = co_department_1
          nurs_treat_ou      = co_ward_1
          transport_type     = co_transport_type_1
        )
    )->set_parameter(
        name          = 'ES_NEW_MOVEMENT_DATA_AT'
        value         = VALUE bapi2097moveat(
          med_emergency_case = co_med_emergency_case_1
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_1 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_addoutpatvisit(
      EXPORTING
        i_institution            = co_institution_id
        i_case_id                = co_case_id
        is_outpat_visit_data     = VALUE #(
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_1
        )
        is_outpat_visit_data_med = VALUE #(
          transport_type         = co_transport_type_1
        )
        is_outpat_visit_data_at  = VALUE #(
          med_emergency_case     = co_med_emergency_case_1
        )
        i_testrun                = abap_true
    ).

    configure_bapi_call( )->set_parameter(
        name          = 'ES_NEW_MOVEMENT_DATA'
        value         = VALUE bapi2097move(
          movemnt_seqno      = co_movement_number_1
          movemnt_type       = co_movement_type_1
          movemnt_date       = co_movement_date_1
          movemnt_time       = co_movement_time_1
          department         = co_department_1
          nurs_treat_ou      = co_ward_1
          transport_type     = co_transport_type_1
        )
    )->set_parameter(
        name          = 'ES_NEW_MOVEMENT_DATA_AT'
        value         = VALUE bapi2097moveat(
          med_emergency_case = co_med_emergency_case_1
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_2 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_addoutpatvisit(
      EXPORTING
        i_institution            = co_institution_id
        i_case_id                = co_case_id
        is_outpat_visit_data     = VALUE #(
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_1
        )
        is_outpat_visit_data_med = VALUE #(
          transport_type         = co_transport_type_1
        )
        is_outpat_visit_data_at  = VALUE #(
          med_emergency_case     = co_med_emergency_case_1
        )
        i_testrun                = abap_false
    ).

    " Es sollten weder Bewegungen geändert noch storniert werden.
    expect_no_changeoutpatvisit( ).
    expect_no_canceloutpatvisit( ).

    " Da wir keine Fehler gemeldet haben, sollte ein Commit und kein Rollback erfolgen.
    expect_transaction_handling(
        i_commit   = abap_true
        i_rollback = abap_false
    ).

    " Nachricht verarbeiten
    connector->send(
        ir_logger            = me->logger
        ir_message           = message
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
    ).

    " es darf zu keinem Fehler gekommen sein
    assert_no_error_message( ).
    assert_process_status(
      i_status = if_ishmed_mci_message_constant=>co_message_status_success
    ).

    " die Testnachrichten müssen aufgezeichnet worden sein
    assert_test_message_logged(
        i_message_number = co_check_message_number_1
    ).
    assert_test_message_logged(
        i_message_number = co_check_message_number_2
    ).

    " alle Test Doubles müssen erwartungsgemäß angesteuert worden sein
    cl_abap_testdouble=>verify_expectations( me->connector->bapi_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->elink_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->history_double ).

  ENDMETHOD.

  METHOD create_cancelled.

    " Eingabenachricht erzeugen
    DATA(message) = create_default_message( ).
    message->set_cancelled( abap_true ).

    " Wir erwarten Anfragen bei der History-API, antworten aber mit Initialwerten
    " (weil wir keine vorherige Nachricht annehmen).
    configure_history_response(
        i_message_date = VALUE d( )
        i_message_time = VALUE t( )
    ).

    " Im Rahmen der Verbuchung muss die History-Tabelle aktualisiert werden.
    expect_history_updated( ).

    " Bei einer Neuanlage gibt es noch kein verknüpftes Objekt.
    configure_elink_response( ).

    " Da die Verbuchung abgebrochen wird, darf die Verknüpfungstabelle nicht aktualisiert werden.
    expect_elink_unchanged( ).

    " Die Sperren der History- und Verknüpfungstabelle müssen aufgehoben werden.
    expect_history_finalized( ).
    expect_elink_finalized( ).

    " Da es keine verknüpften Bewegungen gibt, sollte nicht danach gesucht werden.
    expect_no_getoutpatvisit( ).

    " Es darf keine Bewegung angelegt, geändert oder storniert werden.
    expect_no_addoutpatvisit( ).
    expect_no_changeoutpatvisit( ).
    expect_no_canceloutpatvisit( ).

    " Da wir keine Fehler gemeldet haben, sollte ein Commit und kein Rollback erfolgen.
    " (Verbuchung der History-Tabelle)
    expect_transaction_handling(
        i_commit   = abap_true
        i_rollback = abap_false
    ).

    " Nachricht verarbeiten
    connector->send(
        ir_logger            = me->logger
        ir_message           = message
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
    ).

    " wir erwarten eine Warnung und einen Prozessabbruch
    assert_warning_message( ).
    assert_process_status(
        i_status = if_ishmed_mci_message_constant=>co_message_status_abort
    ).

    " alle Test Doubles müssen erwartungsgemäß angesteuert worden sein
    cl_abap_testdouble=>verify_expectations( me->connector->bapi_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->elink_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->history_double ).

  ENDMETHOD.

  METHOD create_obsolete_date.

    " Nachricht "zurückdatieren"
    me->context->set_message_date( CONV #( sy-datum - 14 ) ).

    " Eingabenachricht erzeugen
    DATA(message) = create_default_message( ).

    " History-API meldet eine aktuellere bereits verarbeitete Nachricht.
    " (weil wir keine vorherige Nachricht annehmen).
    configure_history_response(
        i_message_date = CONV #( sy-datum - 7 )
        i_message_time = sy-uzeit
    ).

    " Im Rahmen der Verbuchung darf die die History-Tabelle nicht aktualisiert werden.
    expect_history_unchanged( ).

    " Es darf nicht nach einer vorhandenen Bewegung gesucht werden.
    expect_elink_unqueried( ).

    " Da die Verbuchung abgebrochen wird, darf die Verknüpfungstabelle nicht aktualisiert werden.
    expect_elink_unchanged( ).

    " Die Sperren der History-Tabelle müssen aufgehoben werden.
    expect_history_finalized( ).

    " Da es keine verknüpften Bewegungen gibt, sollte nicht danach gesucht werden.
    expect_no_getoutpatvisit( ).

    " Es darf keine Bewegung angelegt, geändert oder storniert werden.
    expect_no_addoutpatvisit( ).
    expect_no_changeoutpatvisit( ).
    expect_no_canceloutpatvisit( ).

    " Da keine Aktionen erfolgt sind, darf weder Commit noch Rollback erfolgen.
    expect_transaction_handling(
        i_commit   = abap_false
        i_rollback = abap_false
    ).

    " Nachricht verarbeiten
    connector->send(
        ir_logger            = me->logger
        ir_message           = message
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
    ).

    " wir erwarten eine Warnung und einen Prozessabbruch
    assert_warning_message( ).
    assert_process_status(
        i_status = if_ishmed_mci_message_constant=>co_message_status_abort
    ).

    " alle Test Doubles müssen erwartungsgemäß angesteuert worden sein
    cl_abap_testdouble=>verify_expectations( me->connector->bapi_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->elink_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->history_double ).

  ENDMETHOD.

  METHOD create_obsolete_time.

    " Sicherung gegen Zeitüberlauf
    IF sy-uzeit <= '000100'.
      cl_abap_unit_assert=>abort(
          msg    = 'Can not use current time, try again after 00:01:00'
      ).
    ENDIF.

    " Nachricht "zurückdatieren"
    me->context->set_message_time( CONV #( sy-uzeit - 10 ) ).

    " Eingabenachricht erzeugen
    DATA(message) = create_default_message( ).

    " History-API meldet eine aktuellere bereits verarbeitete Nachricht.
    " (weil wir keine vorherige Nachricht annehmen).
    configure_history_response(
        i_message_date = sy-datum
        i_message_time = CONV #( sy-uzeit - 5 )
    ).

    " Im Rahmen der Verbuchung darf die die History-Tabelle nicht aktualisiert werden.
    expect_history_unchanged( ).

    " Es darf nicht nach einer vorhandenen Bewegung gesucht werden.
    expect_elink_unqueried( ).

    " Da die Verbuchung abgebrochen wird, darf die Verknüpfungstabelle nicht aktualisiert werden.
    expect_elink_unchanged( ).

    " Die Sperren der History-Tabelle müssen aufgehoben werden.
    expect_history_finalized( ).

    " Da es keine verknüpften Bewegungen gibt, sollte nicht danach gesucht werden.
    expect_no_getoutpatvisit( ).

    " Es darf keine weitere Bewegung angelegt werden.
    expect_no_addoutpatvisit( ).

    " Es sollten weder Bewegungen geändert noch storniert werden.
    expect_no_changeoutpatvisit( ).
    expect_no_canceloutpatvisit( ).

    " Da keine Aktionen erfolgt sind, darf weder Commit noch Rollback erfolgen.
    expect_transaction_handling(
        i_commit   = abap_false
        i_rollback = abap_false
    ).

    " Nachricht verarbeiten
    connector->send(
        ir_logger            = me->logger
        ir_message           = message
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
    ).

    " wir erwarten eine Warnung und einen Prozessabbruch
    assert_warning_message( ).
    assert_process_status(
        i_status = if_ishmed_mci_message_constant=>co_message_status_abort
    ).

    " alle Test Doubles müssen erwartungsgemäß angesteuert worden sein
    cl_abap_testdouble=>verify_expectations( me->connector->bapi_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->elink_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->history_double ).

  ENDMETHOD.

  METHOD create_update_cancelled.

    " Eingabenachricht erzeugen
    DATA(message) = create_default_message( ).

    " Wir erwarten Anfragen bei der History-API, antworten aber mit Initialwerten
    " (weil die History hier nicht testen wollen).
    configure_history_response(
        i_message_date = VALUE d( )
        i_message_time = VALUE t( )
    ).

    " Im Rahmen der Verbuchung muss die History-Tabelle aktualisiert werden.
    expect_history_updated( ).

    " Es gibt eine verknüpfte Bewegung, die aber storniert ist (s. u.).
    configure_elink_response(
        i_objects = VALUE #( ( CONV #( |{ co_institution_id }{ co_case_id }{ co_movement_number_1 }| ) ) )
    ).

    " Die Verknüpfungstabelle wird um die neue Bewegung ergänzt.
    expect_elink_updated(
        i_object_id = CONV #( |{ co_institution_id }{ co_case_id }{ co_movement_number_2 }| )
    ).

    " Der Ist-Stand der verknüpften Bewegung muss gelesen werden.
    configure_bapi_call( )->set_parameter(
        name          = 'ES_VISIT_DATA'
        value         = VALUE bapi2097ovout(
          client                 = sy-mandt
          institution            = co_institution_id
          patcaseid              = co_case_id
          movemnt_seqno          = co_movement_number_1
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_1
          transport_type         = co_transport_type_1
          cancel_ind             = abap_true " <<<--- storniert!
        )
    )->set_parameter(
        name          = 'ES_VISIT_DATA_AT'
        value         = VALUE bapi2097atout(
           med_emergency_case     = co_med_emergency_case_1
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_1 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_getoutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
    ).

    " Es sollte eine Bewegung angelegt werden, und zwar zuerst einmal im Testmodus,
    " dann mit Verbuchung.
    configure_bapi_call( )->set_parameter(
        name          = 'ES_NEW_MOVEMENT_DATA'
        value         = VALUE bapi2097move(
          " movemnt_seqno    --> wird im Testmodus nicht vergeben
          movemnt_type       = co_movement_type_1
          movemnt_date       = co_movement_date_1
          movemnt_time       = co_movement_time_1
          department         = co_department_1
          nurs_treat_ou      = co_ward_1
          transport_type     = co_transport_type_1
        )
    )->set_parameter(
        name          = 'ES_NEW_MOVEMENT_DATA_AT'
        value         = VALUE bapi2097moveat(
          med_emergency_case = co_med_emergency_case_1
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_2 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_addoutpatvisit(
      EXPORTING
        i_institution            = co_institution_id
        i_case_id                = co_case_id
        is_outpat_visit_data     = VALUE #(
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_1
        )
        is_outpat_visit_data_med = VALUE #(
          transport_type         = co_transport_type_1
        )
        is_outpat_visit_data_at  = VALUE #(
          med_emergency_case     = co_med_emergency_case_1
        )
        i_testrun                = abap_true
    ).

    configure_bapi_call( )->set_parameter(
        name          = 'ES_NEW_MOVEMENT_DATA'
        value         = VALUE bapi2097move(
          movemnt_seqno      = co_movement_number_2 " <<<--- neue Bewegungsnummer
          movemnt_type       = co_movement_type_1
          movemnt_date       = co_movement_date_1
          movemnt_time       = co_movement_time_1
          department         = co_department_1
          nurs_treat_ou      = co_ward_1
          transport_type     = co_transport_type_1
        )
    )->set_parameter(
        name          = 'ES_NEW_MOVEMENT_DATA_AT'
        value         = VALUE bapi2097moveat(
          med_emergency_case = co_med_emergency_case_1
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_3 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_addoutpatvisit(
      EXPORTING
        i_institution            = co_institution_id
        i_case_id                = co_case_id
        is_outpat_visit_data     = VALUE #(
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_1
        )
        is_outpat_visit_data_med = VALUE #(
          transport_type         = co_transport_type_1
        )
        is_outpat_visit_data_at  = VALUE #(
          med_emergency_case     = co_med_emergency_case_1
        )
        i_testrun                = abap_false
    ).

    " Es sollten weder Bewegungen geändert noch storniert werden.
    expect_no_changeoutpatvisit( ).
    expect_no_canceloutpatvisit( ).

    " Da wir keine Fehler gemeldet haben, sollte ein Commit und kein Rollback erfolgen.
    expect_transaction_handling(
        i_commit   = abap_true
        i_rollback = abap_false
    ).

    " Nachricht verarbeiten
    connector->send(
        ir_logger            = me->logger
        ir_message           = message
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
    ).

    " es darf zu keinem Fehler gekommen sein
    assert_no_error_message( ).
    assert_process_status(
      i_status = if_ishmed_mci_message_constant=>co_message_status_success
    ).

    " alle Test Doubles müssen erwartungsgemäß angesteuert worden sein
    cl_abap_testdouble=>verify_expectations( me->connector->bapi_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->elink_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->history_double ).

    " die Testnachrichten müssen aufgezeichnet worden sein
    assert_test_message_logged(
        i_message_number = co_check_message_number_1
    ).
    assert_test_message_logged(
        i_message_number = co_check_message_number_2
    ).
    assert_test_message_logged(
        i_message_number = co_check_message_number_3
    ).

  ENDMETHOD.

  METHOD error_test.

    " Eingabenachricht erzeugen
    DATA(message) = create_default_message( ).

    " Wir erwarten Anfragen bei der History-API, antworten aber mit Initialwerten
    " (weil wir keine vorherige Nachricht annehmen).
    configure_history_response( ).

    " Im Rahmen der Verbuchung darf die die History-Tabelle nicht aktualisiert werden.
    expect_history_unchanged( ).

    " Bei einer Neuanlage gibt es noch kein verknüpftes Objekt.
    configure_elink_response( ).

    " Da die Verbuchung abgebrochen wird, darf die Verknüpfungstabelle nicht aktualisiert werden.
    expect_elink_unchanged( ).

    " Die Sperren der History- und Verknüpfungstabelle müssen aufgehoben werden.
    expect_history_finalized( ).
    expect_elink_finalized( ).

    " Da es keine verknüpften Bewegungen gibt, sollte nicht danach gesucht werden.
    expect_no_getoutpatvisit( ).

    " Es sollte eine Bewegung angelegt werden, und zwar zuerst einmal im Testmodus,
    " da soll aber schon ein Fehler kommen.
    configure_bapi_call( )->set_parameter(
        name          = 'ES_NEW_MOVEMENT_DATA'
        value         = VALUE bapi2097move(
          " movemnt_seqno    --> wird im Testmodus nicht vergeben
          movemnt_type       = co_movement_type_1
          movemnt_date       = co_movement_date_1
          movemnt_time       = co_movement_time_1
          department         = co_department_1
          nurs_treat_ou      = co_ward_1
          transport_type     = co_transport_type_1
        )
    )->set_parameter(
        name          = 'ES_NEW_MOVEMENT_DATA_AT'
        value         = VALUE bapi2097moveat(
          med_emergency_case = co_med_emergency_case_1
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = 'E'
            id     = co_check_message_id
            number = co_check_message_number_1 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = 'E'
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_addoutpatvisit(
      EXPORTING
        i_institution            = co_institution_id
        i_case_id                = co_case_id
        is_outpat_visit_data     = VALUE #(
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_1
        )
        is_outpat_visit_data_med = VALUE #(
          transport_type         = co_transport_type_1
        )
        is_outpat_visit_data_at  = VALUE #(
          med_emergency_case     = co_med_emergency_case_1
        )
        i_testrun                = abap_true
    ).

    " Es sollten weder Bewegungen geändert noch storniert werden.
    expect_no_changeoutpatvisit( ).
    expect_no_canceloutpatvisit( ).

    " Da wir einen Fehler gemeldet haben, sollte ein Rollback erfolgen.
    expect_transaction_handling(
        i_commit   = abap_false
        i_rollback = abap_true
    ).

    " Nachricht verarbeiten
    connector->send(
        ir_logger            = me->logger
        ir_message           = message
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
    ).

    " es muss zu einem Fehler gekommen sein
    assert_error_message( ).
    assert_process_status(
        i_status = if_ishmed_mci_message_constant=>co_message_status_error
    ).

    " die Testnachricht muss aufgezeichnet worden sein
    assert_test_message_logged(
        i_message_type   = 'E'
        i_message_number = co_check_message_number_1
    ).

    " alle Test Doubles müssen erwartungsgemäß angesteuert worden sein
    cl_abap_testdouble=>verify_expectations( me->connector->bapi_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->elink_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->history_double ).

  ENDMETHOD.

  METHOD error_update.

    " Eingabenachricht erzeugen
    DATA(message) = create_default_message( ).

    " Wir erwarten Anfragen bei der History-API, antworten aber mit Initialwerten
    " (weil wir keine vorherige Nachricht annehmen).
    configure_history_response( ).

    " Im Rahmen der Verbuchung darf die die History-Tabelle nicht aktualisiert werden.
    expect_history_unchanged( ).

    " Bei einer Neuanlage gibt es noch kein verknüpftes Objekt.
    configure_elink_response( ).

    " Da die Verbuchung abgebrochen wird, darf die Verknüpfungstabelle nicht aktualisiert werden.
    expect_elink_unchanged( ).

    " Die Sperren der History- und Verknüpfungstabelle müssen aufgehoben werden.
    expect_history_finalized( ).
    expect_elink_finalized( ).

    " Da es keine verknüpften Bewegungen gibt, sollte nicht danach gesucht werden.
    expect_no_getoutpatvisit( ).

    " Es sollte eine Bewegung angelegt werden, und zwar zuerst einmal im Testmodus,
    " dann mit Verbuchung, dabei soll es dann zu einem Fehler kommen.
    configure_bapi_call( )->set_parameter(
        name          = 'ES_NEW_MOVEMENT_DATA'
        value         = VALUE bapi2097move(
          " movemnt_seqno    --> wird im Testmodus nicht vergeben
          movemnt_type       = co_movement_type_1
          movemnt_date       = co_movement_date_1
          movemnt_time       = co_movement_time_1
          department         = co_department_1
          nurs_treat_ou      = co_ward_1
          transport_type     = co_transport_type_1
        )
    )->set_parameter(
        name          = 'ES_NEW_MOVEMENT_DATA_AT'
        value         = VALUE bapi2097moveat(
          med_emergency_case = co_med_emergency_case_1
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_1 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_addoutpatvisit(
      EXPORTING
        i_institution            = co_institution_id
        i_case_id                = co_case_id
        is_outpat_visit_data     = VALUE #(
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_1
        )
        is_outpat_visit_data_med = VALUE #(
          transport_type         = co_transport_type_1
        )
        is_outpat_visit_data_at  = VALUE #(
          med_emergency_case     = co_med_emergency_case_1
        )
        i_testrun                = abap_true
    ).

    configure_bapi_call( )->set_parameter(
        name          = 'ES_NEW_MOVEMENT_DATA'
        value         = VALUE bapi2097move(
          movemnt_seqno      = co_movement_number_1
          movemnt_type       = co_movement_type_1
          movemnt_date       = co_movement_date_1
          movemnt_time       = co_movement_time_1
          department         = co_department_1
          nurs_treat_ou      = co_ward_1
          transport_type     = co_transport_type_1
        )
    )->set_parameter(
        name          = 'ES_NEW_MOVEMENT_DATA_AT'
        value         = VALUE bapi2097moveat(
          med_emergency_case = co_med_emergency_case_1
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = 'E'
            id     = co_check_message_id
            number = co_check_message_number_2 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = 'E'
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_addoutpatvisit(
      EXPORTING
        i_institution            = co_institution_id
        i_case_id                = co_case_id
        is_outpat_visit_data     = VALUE #(
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_1
        )
        is_outpat_visit_data_med = VALUE #(
          transport_type         = co_transport_type_1
        )
        is_outpat_visit_data_at  = VALUE #(
          med_emergency_case     = co_med_emergency_case_1
        )
        i_testrun                = abap_false
    ).

    " Es sollten weder Bewegungen geändert noch storniert werden.
    expect_no_changeoutpatvisit( ).
    expect_no_canceloutpatvisit( ).

    " Da wir einen Fehler gemeldet haben, sollte ein Rollback erfolgen.
    expect_transaction_handling(
        i_commit   = abap_false
        i_rollback = abap_true
    ).

    " Nachricht verarbeiten
    connector->send(
        ir_logger            = me->logger
        ir_message           = message
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
    ).

    " es muss zu einem Fehler gekommen sein
    assert_error_message( ).
    assert_process_status(
        i_status = if_ishmed_mci_message_constant=>co_message_status_error
    ).

    " die Testnachrichten müssen aufgezeichnet worden sein
    assert_test_message_logged(
        i_message_number = co_check_message_number_1
    ).
    assert_test_message_logged(
        i_message_type   = 'E'
        i_message_number = co_check_message_number_2
    ).

    " alle Test Doubles müssen erwartungsgemäß angesteuert worden sein
    cl_abap_testdouble=>verify_expectations( me->connector->bapi_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->elink_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->history_double ).

  ENDMETHOD.



ENDCLASS.

**
** Unit Tests for movement update
**
CLASS lcl_test_update DEFINITION FINAL
  INHERITING FROM lcl_unit_test_base
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PROTECTED SECTION.
    "! Test einer Bewegungsaktualisierung (IS-H-Kerndaten).
    METHODS update_admin FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Test einer Bewegungsaktualisierung (medizinische Daten).
    METHODS update_med FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Test einer Bewegungsaktualisierung (Zusatzdaten Landesversion AT).
    METHODS update_cv_at FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Test der Fehlerbehandlung bei der Aktualisierung: Fehler beim
    "! Lesen der Bewegungen
    METHODS error_movement_read FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Test der Fehlerbehandlung bei der Aktualisierung: Fehler im Testmodus
    METHODS error_test FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Test der Fehlerbehandlung bei der Aktualisierung: Fehler im Verbuchungsmodus
    METHODS error_update FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Test der Fehlerbehandlung bei der Aktualisierung: es gibt mehrere
    "! zugeordnete Bewegungen (Inkonsistenz).
    METHODS error_multiple_movements FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

ENDCLASS.

CLASS lcl_test_update IMPLEMENTATION.

  METHOD update_admin.

    " Eingabenachricht erzeugen
    DATA(message) = create_default_message( ).

    " Wir erwarten Anfragen bei der History-API, antworten aber mit Initialwerten
    " (weil die History hier nicht testen wollen).
    configure_history_response(
        i_message_date = VALUE d( )
        i_message_time = VALUE t( )
    ).

    " Im Rahmen der Verbuchung muss die History-Tabelle aktualisiert werden.
    expect_history_updated( ).

    " Es gibt eine verknüpfte Bewegung.
    configure_elink_response(
        i_objects = VALUE #( ( CONV #( |{ co_institution_id }{ co_case_id }{ co_movement_number_1 }| ) ) )
    ).

    " Die Verknüpfungstabelle wird nicht geändert.
    expect_elink_unchanged( ).

    " Der Ist-Stand der verknüpften Bewegung muss gelesen werden.
    configure_bapi_call( )->set_parameter(
        name          = 'ES_VISIT_DATA'
        value         = VALUE bapi2097ovout(
          client                 = sy-mandt
          institution            = co_institution_id
          patcaseid              = co_case_id
          movemnt_seqno          = co_movement_number_1
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_2 " <<<--- changed!
          transport_type         = co_transport_type_1
        )
    )->set_parameter(
        name          = 'ES_VISIT_DATA_AT'
        value         = VALUE bapi2097atout(
           med_emergency_case     = co_med_emergency_case_1
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_1 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_getoutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
    ).

    " Es sollte eine Bewegung geändert werden, und zwar zuerst einmal im Testmodus,
    " dann mit Verbuchung. Wir liefern hier die Ausgabeparameter nicht mit, weil sie
    " nicht weiter verarbeitet werden sollten.
    configure_bapi_call( )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_2 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_changeoutpatvisit(
      EXPORTING
        i_institution               = co_institution_id
        i_case_id                   = co_case_id
        i_movement_number           = co_movement_number_1
        is_changed_visit_data       = VALUE #(
          nurs_treat_ou  = co_ward_1
          nurs_treat_oux = abap_true
        )
        is_changed_visit_data_med   = VALUE #(
        )
        is_changed_visit_data_at    = VALUE #(
        )
        i_testrun                   = abap_true
    ).

    configure_bapi_call( )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_3 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_changeoutpatvisit(
      EXPORTING
        i_institution               = co_institution_id
        i_case_id                   = co_case_id
        i_movement_number           = co_movement_number_1
        is_changed_visit_data       = VALUE #(
          nurs_treat_ou  = co_ward_1
          nurs_treat_oux = abap_true
        )
        is_changed_visit_data_med   = VALUE #(
        )
        is_changed_visit_data_at    = VALUE #(
        )
        i_testrun                   = abap_false
    ).

    " Es sollten weder Bewegungen angelegt noch storniert werden.
    expect_no_addoutpatvisit( ).
    expect_no_canceloutpatvisit( ).

    " Da wir keine Fehler gemeldet haben, sollte ein Commit und kein Rollback erfolgen.
    expect_transaction_handling(
        i_commit   = abap_true
        i_rollback = abap_false
    ).

    " Nachricht verarbeiten
    connector->send(
        ir_logger            = me->logger
        ir_message           = message
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
    ).

    " es darf zu keinem Fehler gekommen sein
    assert_no_error_message( ).
    assert_process_status(
      i_status = if_ishmed_mci_message_constant=>co_message_status_success
    ).

    " alle Test Doubles müssen erwartungsgemäß angesteuert worden sein
    cl_abap_testdouble=>verify_expectations( me->connector->bapi_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->elink_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->history_double ).

    " die Testnachrichten müssen aufgezeichnet worden sein
    assert_test_message_logged(
        i_message_number = co_check_message_number_1
    ).
    assert_test_message_logged(
        i_message_number = co_check_message_number_2
    ).
    assert_test_message_logged(
        i_message_number = co_check_message_number_3
    ).

  ENDMETHOD.

  METHOD update_med.

    " Eingabenachricht erzeugen
    DATA(message) = create_default_message( ).

    " Wir erwarten Anfragen bei der History-API, antworten aber mit Initialwerten
    " (weil die History hier nicht testen wollen).
    configure_history_response(
        i_message_date = VALUE d( )
        i_message_time = VALUE t( )
    ).

    " Im Rahmen der Verbuchung muss die History-Tabelle aktualisiert werden.
    expect_history_updated( ).

    " Es gibt eine verknüpfte Bewegung.
    configure_elink_response(
        i_objects = VALUE #( ( CONV #( |{ co_institution_id }{ co_case_id }{ co_movement_number_1 }| ) ) )
    ).

    " Die Verknüpfungstabelle wird nicht geändert.
    expect_elink_unchanged( ).

    " Der Ist-Stand der verknüpften Bewegung muss gelesen werden.
    configure_bapi_call( )->set_parameter(
        name          = 'ES_VISIT_DATA'
        value         = VALUE bapi2097ovout(
          client                 = sy-mandt
          institution            = co_institution_id
          patcaseid              = co_case_id
          movemnt_seqno          = co_movement_number_1
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_1
          transport_type         = co_transport_type_2 " <<<--- changed!
        )
    )->set_parameter(
        name          = 'ES_VISIT_DATA_AT'
        value         = VALUE bapi2097atout(
           med_emergency_case     = co_med_emergency_case_1
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_1 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).

    me->connector->bapi_double->call_patcase_getoutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
    ).


    " Es sollte eine Bewegung geändert werden, und zwar zuerst einmal im Testmodus,
    " dann mit Verbuchung. Wir liefern hier die Ausgabeparameter nicht mit, weil sie
    " nicht weiter verarbeitet werden sollten.
    configure_bapi_call( )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_2 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_changeoutpatvisit(
      EXPORTING
        i_institution               = co_institution_id
        i_case_id                   = co_case_id
        i_movement_number           = co_movement_number_1
        is_changed_visit_data       = VALUE #(
        )
        is_changed_visit_data_med   = VALUE #(
          transport_type  = co_transport_type_1
          transport_typex = abap_true
        )
        is_changed_visit_data_at    = VALUE #(
        )
        i_testrun                   = abap_true
    ).

    configure_bapi_call( )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_3 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_changeoutpatvisit(
      EXPORTING
        i_institution               = co_institution_id
        i_case_id                   = co_case_id
        i_movement_number           = co_movement_number_1
        is_changed_visit_data       = VALUE #(
        )
        is_changed_visit_data_med   = VALUE #(
          transport_type  = co_transport_type_1
          transport_typex = abap_true
        )
        is_changed_visit_data_at    = VALUE #(
        )
        i_testrun                   = abap_false
    ).

    " Es sollten weder Bewegungen angelegt noch storniert werden.
    expect_no_addoutpatvisit( ).
    expect_no_canceloutpatvisit( ).

    " Da wir keine Fehler gemeldet haben, sollte ein Commit und kein Rollback erfolgen.
    expect_transaction_handling(
        i_commit   = abap_true
        i_rollback = abap_false
    ).

    " Nachricht verarbeiten
    connector->send(
        ir_logger            = me->logger
        ir_message           = message
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
    ).

    " es darf zu keinem Fehler gekommen sein
    assert_no_error_message( ).
    assert_process_status(
      i_status = if_ishmed_mci_message_constant=>co_message_status_success
    ).

    " alle Test Doubles müssen erwartungsgemäß angesteuert worden sein
    cl_abap_testdouble=>verify_expectations( me->connector->bapi_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->elink_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->history_double ).

    " die Testnachrichten müssen aufgezeichnet worden sein
    assert_test_message_logged(
        i_message_number = co_check_message_number_1
    ).
    assert_test_message_logged(
        i_message_number = co_check_message_number_2
    ).
    assert_test_message_logged(
        i_message_number = co_check_message_number_3
    ).

  ENDMETHOD.

  METHOD update_cv_at.

    " Eingabenachricht erzeugen
    DATA(message) = create_default_message( ).

    " Wir erwarten Anfragen bei der History-API, antworten aber mit Initialwerten
    " (weil die History hier nicht testen wollen).
    configure_history_response(
        i_message_date = VALUE d( )
        i_message_time = VALUE t( )
    ).

    " Im Rahmen der Verbuchung muss die History-Tabelle aktualisiert werden.
    expect_history_updated( ).

    " Es gibt eine verknüpfte Bewegung.
    configure_elink_response(
        i_objects = VALUE #( ( CONV #( |{ co_institution_id }{ co_case_id }{ co_movement_number_1 }| ) ) )
    ).

    " Die Verknüpfungstabelle wird nicht geändert.
    expect_elink_unchanged( ).

    " Der Ist-Stand der verknüpften Bewegung muss gelesen werden.
    configure_bapi_call( )->set_parameter(
        name          = 'ES_VISIT_DATA'
        value         = VALUE bapi2097ovout(
          client                 = sy-mandt
          institution            = co_institution_id
          patcaseid              = co_case_id
          movemnt_seqno          = co_movement_number_1
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_1
          transport_type         = co_transport_type_1
        )
    )->set_parameter(
        name          = 'ES_VISIT_DATA_AT'
        value         = VALUE bapi2097atout(
           med_emergency_case     = co_med_emergency_case_2 " <<<--- changed!
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_1 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).

    me->connector->bapi_double->call_patcase_getoutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
    ).


    " Es sollte eine Bewegung geändert werden, und zwar zuerst einmal im Testmodus,
    " dann mit Verbuchung. Wir liefern hier die Ausgabeparameter nicht mit, weil sie
    " nicht weiter verarbeitet werden sollten.
    configure_bapi_call( )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_2 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_changeoutpatvisit(
      EXPORTING
        i_institution               = co_institution_id
        i_case_id                   = co_case_id
        i_movement_number           = co_movement_number_1
        is_changed_visit_data       = VALUE #(
        )
        is_changed_visit_data_med   = VALUE #(
        )
        is_changed_visit_data_at    = VALUE #(
          med_emergency_case   = co_med_emergency_case_1
          med_emergency_casex  = abap_true
        )
        i_testrun                   = abap_true
    ).

    configure_bapi_call( )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_3 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_changeoutpatvisit(
      EXPORTING
        i_institution               = co_institution_id
        i_case_id                   = co_case_id
        i_movement_number           = co_movement_number_1
        is_changed_visit_data       = VALUE #(
        )
        is_changed_visit_data_med   = VALUE #(
        )
        is_changed_visit_data_at    = VALUE #(
          med_emergency_case   = co_med_emergency_case_1
          med_emergency_casex  = abap_true
        )
        i_testrun                   = abap_false
    ).

    " Es sollten weder Bewegungen angelegt noch storniert werden.
    expect_no_addoutpatvisit( ).
    expect_no_canceloutpatvisit( ).

    " Da wir keine Fehler gemeldet haben, sollte ein Commit und kein Rollback erfolgen.
    expect_transaction_handling(
        i_commit   = abap_true
        i_rollback = abap_false
    ).

    " Nachricht verarbeiten
    connector->send(
        ir_logger            = me->logger
        ir_message           = message
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
    ).

    " es darf zu keinem Fehler gekommen sein
    assert_no_error_message( ).
    assert_process_status(
      i_status = if_ishmed_mci_message_constant=>co_message_status_success
    ).

    " alle Test Doubles müssen erwartungsgemäß angesteuert worden sein
    cl_abap_testdouble=>verify_expectations( me->connector->bapi_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->elink_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->history_double ).

    " die Testnachrichten müssen aufgezeichnet worden sein
    assert_test_message_logged(
        i_message_number = co_check_message_number_1
    ).
    assert_test_message_logged(
        i_message_number = co_check_message_number_2
    ).
    assert_test_message_logged(
        i_message_number = co_check_message_number_3
    ).

  ENDMETHOD.

  METHOD error_movement_read.

    " Eingabenachricht erzeugen
    DATA(message) = create_default_message( ).

    " Wir erwarten Anfragen bei der History-API, antworten aber mit Initialwerten
    " (weil die History hier nicht testen wollen).
    configure_history_response(
        i_message_date = VALUE d( )
        i_message_time = VALUE t( )
    ).

    " Aufgrund des Fehlers darf die History-Tabelle nicht aktualisiert werden.
    expect_history_unchanged( ).

    " Es gibt eine verknüpfte Bewegung.
    configure_elink_response(
        i_objects = VALUE #( ( CONV #( |{ co_institution_id }{ co_case_id }{ co_movement_number_1 }| ) ) )
    ).

    " Die Verknüpfungstabelle wird nicht geändert.
    expect_elink_unchanged( ).

    " Der Leseversuch des Ist-Stands der verknüpften Bewegung liefert einen Fehler.
    configure_bapi_call( )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = 'E'
            id     = co_check_message_id
            number = co_check_message_number_1 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = 'E'
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_getoutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
    ).

    " Es sollten weder Bewegungen angelegt noch verändert noch storniert werden.
    expect_no_addoutpatvisit( ).
    expect_no_changeoutpatvisit( ).
    expect_no_canceloutpatvisit( ).

    " Da wir noch nicht in die Verbuchung gegangen sind, sollte weder COMMIT noch
    " ROLLBACK erfolgen.
    expect_transaction_handling(
        i_commit   = abap_false
        i_rollback = abap_false
    ).

    " Nachricht verarbeiten
    TRY.
        connector->send(
            ir_logger            = me->logger
            ir_message           = message
            ir_message_context   = me->context
            ir_runtime_parameter = me->runtime_params
        ).
      CATCH cx_ishmed_mci INTO DATA(mci_exception) ##no_handler.
    ENDTRY.

    " es muss zu einem Fehler gekommen sein, oder der Prozess muss abgebrochen sein
    IF mci_exception IS INITIAL.
      assert_error_message( ).
      assert_process_status(
        i_status = if_ishmed_mci_message_constant=>co_message_status_error
      ).
    ENDIF.

    " alle Test Doubles müssen erwartungsgemäß angesteuert worden sein
    cl_abap_testdouble=>verify_expectations( me->connector->bapi_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->elink_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->history_double ).

    " die Testnachrichten müssen aufgezeichnet worden sein
    assert_test_message_logged(
        i_message_number = co_check_message_number_1
        i_message_type   = 'E'
    ).

  ENDMETHOD.

  METHOD error_test.

    " Eingabenachricht erzeugen
    DATA(message) = create_default_message( ).

    " Wir erwarten Anfragen bei der History-API, antworten aber mit Initialwerten
    " (weil die History hier nicht testen wollen).
    configure_history_response(
        i_message_date = VALUE d( )
        i_message_time = VALUE t( )
    ).

    " Aufgrund des Fehlers darf die History-Tabelle nicht aktualisiert werden.
    expect_history_unchanged( ).

    " Es gibt eine verknüpfte Bewegung.
    configure_elink_response(
        i_objects = VALUE #( ( CONV #( |{ co_institution_id }{ co_case_id }{ co_movement_number_1 }| ) ) )
    ).

    " Die Verknüpfungstabelle wird nicht geändert.
    expect_elink_unchanged( ).

    " Der Ist-Stand der verknüpften Bewegung muss gelesen werden.
    configure_bapi_call( )->set_parameter(
        name          = 'ES_VISIT_DATA'
        value         = VALUE bapi2097ovout(
          client                 = sy-mandt
          institution            = co_institution_id
          patcaseid              = co_case_id
          movemnt_seqno          = co_movement_number_1
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_2 " <<<--- changed!
          transport_type         = co_transport_type_1
        )
    )->set_parameter(
        name          = 'ES_VISIT_DATA_AT'
        value         = VALUE bapi2097atout(
           med_emergency_case     = co_med_emergency_case_1
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_1 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_getoutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
    ).

    " Es sollte eine Bewegung geändert werden, und zwar zuerst einmal im Testmodus
    " - da werfen wir schon einen Fehler
    configure_bapi_call( )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = 'E'
            id     = co_check_message_id
            number = co_check_message_number_2 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = 'E'
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_changeoutpatvisit(
      EXPORTING
        i_institution               = co_institution_id
        i_case_id                   = co_case_id
        i_movement_number           = co_movement_number_1
        is_changed_visit_data       = VALUE #(
          nurs_treat_ou  = co_ward_1
          nurs_treat_oux = abap_true
        )
        is_changed_visit_data_med   = VALUE #(
        )
        is_changed_visit_data_at    = VALUE #(
        )
        i_testrun                   = abap_true
    ).

    " Es sollten weder Bewegungen angelegt noch storniert werden.
    expect_no_addoutpatvisit( ).
    expect_no_canceloutpatvisit( ).

    " Aufgrund des Fehlers erwarten wir ein ROLLBACK.
    expect_transaction_handling(
        i_commit   = abap_false
        i_rollback = abap_true
    ).

    " Nachricht verarbeiten
    TRY.
        connector->send(
            ir_logger            = me->logger
            ir_message           = message
            ir_message_context   = me->context
            ir_runtime_parameter = me->runtime_params
        ).
      CATCH cx_ishmed_mci INTO DATA(mci_exception) ##no_handler.
    ENDTRY.

    " es muss zu einem Fehler gekommen sein, oder der Prozess muss abgebrochen sein
    IF mci_exception IS INITIAL.
      assert_error_message( ).
      assert_process_status(
        i_status = if_ishmed_mci_message_constant=>co_message_status_error
      ).
    ENDIF.

    " alle Test Doubles müssen erwartungsgemäß angesteuert worden sein
    cl_abap_testdouble=>verify_expectations( me->connector->bapi_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->elink_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->history_double ).

    " die Testnachrichten müssen aufgezeichnet worden sein
    assert_test_message_logged(
        i_message_number = co_check_message_number_1
    ).
    assert_test_message_logged(
        i_message_number = co_check_message_number_2
        i_message_type   = 'E'
    ).

  ENDMETHOD.

  METHOD error_update.

    " Eingabenachricht erzeugen
    DATA(message) = create_default_message( ).

    " Wir erwarten Anfragen bei der History-API, antworten aber mit Initialwerten
    " (weil die History hier nicht testen wollen).
    configure_history_response(
        i_message_date = VALUE d( )
        i_message_time = VALUE t( )
    ).

    " Aufgrund des Fehlers darf die History-Tabelle nicht aktualisiert werden.
    expect_history_unchanged( ).

    " Es gibt eine verknüpfte Bewegung.
    configure_elink_response(
        i_objects = VALUE #( ( CONV #( |{ co_institution_id }{ co_case_id }{ co_movement_number_1 }| ) ) )
    ).

    " Die Verknüpfungstabelle wird nicht geändert.
    expect_elink_unchanged( ).

    " Der Ist-Stand der verknüpften Bewegung muss gelesen werden.
    configure_bapi_call( )->set_parameter(
        name          = 'ES_VISIT_DATA'
        value         = VALUE bapi2097ovout(
          client                 = sy-mandt
          institution            = co_institution_id
          patcaseid              = co_case_id
          movemnt_seqno          = co_movement_number_1
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_2 " <<<--- changed!
          transport_type         = co_transport_type_1
        )
    )->set_parameter(
        name          = 'ES_VISIT_DATA_AT'
        value         = VALUE bapi2097atout(
           med_emergency_case     = co_med_emergency_case_1
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_1 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_getoutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
    ).

    " Es sollte eine Bewegung geändert werden, und zwar zuerst einmal im Testmodus,
    " dann mit Verbuchung - da werfen wir dann einen Fehler.
    configure_bapi_call( )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_2 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_changeoutpatvisit(
      EXPORTING
        i_institution               = co_institution_id
        i_case_id                   = co_case_id
        i_movement_number           = co_movement_number_1
        is_changed_visit_data       = VALUE #(
          nurs_treat_ou  = co_ward_1
          nurs_treat_oux = abap_true
        )
        is_changed_visit_data_med   = VALUE #(
        )
        is_changed_visit_data_at    = VALUE #(
        )
        i_testrun                   = abap_true
    ).

    configure_bapi_call( )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = 'E'
            id     = co_check_message_id
            number = co_check_message_number_3 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = 'E'
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_changeoutpatvisit(
      EXPORTING
        i_institution               = co_institution_id
        i_case_id                   = co_case_id
        i_movement_number           = co_movement_number_1
        is_changed_visit_data       = VALUE #(
          nurs_treat_ou  = co_ward_1
          nurs_treat_oux = abap_true
        )
        is_changed_visit_data_med   = VALUE #(
        )
        is_changed_visit_data_at    = VALUE #(
        )
        i_testrun                   = abap_false
    ).

    " Es sollten weder Bewegungen angelegt noch storniert werden.
    expect_no_addoutpatvisit( ).
    expect_no_canceloutpatvisit( ).

    " Aufgrund des Fehlers erwarten wir ein ROLLBACK.
    expect_transaction_handling(
        i_commit   = abap_false
        i_rollback = abap_true
    ).

    " Nachricht verarbeiten
    TRY.
        connector->send(
            ir_logger            = me->logger
            ir_message           = message
            ir_message_context   = me->context
            ir_runtime_parameter = me->runtime_params
        ).
      CATCH cx_ishmed_mci INTO DATA(mci_exception) ##no_handler.
    ENDTRY.

    " es muss zu einem Fehler gekommen sein, oder der Prozess muss abgebrochen sein
    IF mci_exception IS INITIAL.
      assert_error_message( ).
      assert_process_status(
        i_status = if_ishmed_mci_message_constant=>co_message_status_error
      ).
    ENDIF.

    " alle Test Doubles müssen erwartungsgemäß angesteuert worden sein
    cl_abap_testdouble=>verify_expectations( me->connector->bapi_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->elink_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->history_double ).

    " die Testnachrichten müssen aufgezeichnet worden sein
    assert_test_message_logged(
        i_message_number = co_check_message_number_1
    ).
    assert_test_message_logged(
        i_message_number = co_check_message_number_2
    ).
    assert_test_message_logged(
        i_message_number = co_check_message_number_3
        i_message_type   = 'E'
    ).

  ENDMETHOD.

  METHOD error_multiple_movements.

    " Eingabenachricht erzeugen
    DATA(message) = create_default_message( ).

    " Wir erwarten Anfragen bei der History-API, antworten aber mit Initialwerten
    " (weil die History hier nicht testen wollen).
    configure_history_response(
        i_message_date = VALUE d( )
        i_message_time = VALUE t( )
    ).

    " Aufgrund des Fehlers darf die History-Tabelle nicht aktualisiert werden.
    expect_history_unchanged( ).

    " Es gibt ZWEI verknüpfte Bewegungen.
    configure_elink_response(
        i_objects = VALUE #(
          ( CONV #( |{ co_institution_id }{ co_case_id }{ co_movement_number_1 }| ) )
          ( CONV #( |{ co_institution_id }{ co_case_id }{ co_movement_number_2 }| ) )
        )
    ).

    " Die Verknüpfungstabelle wird nicht geändert.
    expect_elink_unchanged( ).

    " Der Ist-Stand der verknüpften Bewegungen muss gelesen werden - beide sind
    " aktiv.
    configure_bapi_call( )->set_parameter(
        name          = 'ES_VISIT_DATA'
        value         = VALUE bapi2097ovout(
          client                 = sy-mandt
          institution            = co_institution_id
          patcaseid              = co_case_id
          movemnt_seqno          = co_movement_number_1
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_1
          transport_type         = co_transport_type_1
        )
    )->set_parameter(
        name          = 'ES_VISIT_DATA_AT'
        value         = VALUE bapi2097atout(
           med_emergency_case     = co_med_emergency_case_1
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_1 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_getoutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
    ).

    configure_bapi_call( )->set_parameter(
        name          = 'ES_VISIT_DATA'
        value         = VALUE bapi2097ovout(
          client                 = sy-mandt
          institution            = co_institution_id
          patcaseid              = co_case_id
          movemnt_seqno          = co_movement_number_2
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_2
          transport_type         = co_transport_type_2
        )
    )->set_parameter(
        name          = 'ES_VISIT_DATA_AT'
        value         = VALUE bapi2097atout(
           med_emergency_case     = co_med_emergency_case_2
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_2 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_getoutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_2
    ).

    " Es sollten weder Bewegungen angelegt noch verändert noch storniert werden.
    expect_no_addoutpatvisit( ).
    expect_no_changeoutpatvisit( ).
    expect_no_canceloutpatvisit( ).

    " Da wir noch nicht in die Verbuchung gegangen sind, sollte weder COMMIT noch
    " ROLLBACK erfolgen.
    expect_transaction_handling(
        i_commit   = abap_false
        i_rollback = abap_false
    ).

    " Nachricht verarbeiten
    TRY.
        connector->send(
            ir_logger            = me->logger
            ir_message           = message
            ir_message_context   = me->context
            ir_runtime_parameter = me->runtime_params
        ).
      CATCH cx_ishmed_mci INTO DATA(mci_exception) ##no_handler.
    ENDTRY.

    " es muss zu einem Fehler gekommen sein, oder der Prozess muss abgebrochen sein
    IF mci_exception IS INITIAL.
      assert_error_message( ).
      assert_process_status(
        i_status = if_ishmed_mci_message_constant=>co_message_status_error
      ).
    ENDIF.

    " alle Test Doubles müssen erwartungsgemäß angesteuert worden sein
    cl_abap_testdouble=>verify_expectations( me->connector->bapi_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->elink_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->history_double ).

    " die Testnachrichten müssen aufgezeichnet worden sein
    assert_test_message_logged(
        i_message_number = co_check_message_number_1
    ).
    assert_test_message_logged(
        i_message_number = co_check_message_number_2
    ).

  ENDMETHOD.

ENDCLASS.


**
** Unit Tests for movement cancellation
**
CLASS lcl_test_cancel DEFINITION FINAL
  INHERITING FROM lcl_unit_test_base
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PROTECTED SECTION.

    "! Test eines Bewegungsstornos.
    METHODS cancel_normal FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Test des Umgangs mit einer bereits stornierten Bewegung
    METHODS cancel_already_cancelled FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Test der Fehlerbehandlung bei Storno: Fehler beim Lesen der Bewegungen
    "! (tritt z. B. auch beim Lesen einer nicht vorhandenen Bewegung auf)
    METHODS error_movement_read FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Test der Fehlerbehandlung bei Storno: Fehler im Testmodus
    METHODS error_test FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Test der Fehlerbehandlung bei Storno: Fehler im Verbuchungsmodus
    METHODS error_update FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    "! Test der Fehlerbehandlung bei Storno: es gibt mehrere
    "! zugeordnete Bewegungen (Inkonsistenz).
    METHODS error_multiple_movements FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

ENDCLASS.

CLASS lcl_test_cancel IMPLEMENTATION.

  METHOD cancel_normal.

    " Eingabenachricht erzeugen und Stornokennzeichen setzen.
    DATA(message) = create_default_message( ).
    message->set_cancelled( abap_true ).

    " Wir erwarten Anfragen bei der History-API, antworten aber mit Initialwerten
    " (weil wir die History hier nicht testen wollen).
    configure_history_response(
        i_message_date = VALUE d( )
        i_message_time = VALUE t( )
    ).

    " Im Rahmen der Verbuchung muss die History-Tabelle aktualisiert werden.
    expect_history_updated( ).

    " Es gibt eine verknüpfte Bewegung.
    configure_elink_response(
        i_objects = VALUE #( ( CONV #( |{ co_institution_id }{ co_case_id }{ co_movement_number_1 }| ) ) )
    ).

    " Die Verknüpfungstabelle wird nicht geändert.
    expect_elink_unchanged( ).

    " Der Ist-Stand der verknüpften Bewegung muss gelesen werden.
    configure_bapi_call( )->set_parameter(
        name          = 'ES_VISIT_DATA'
        value         = VALUE bapi2097ovout(
          client                 = sy-mandt
          institution            = co_institution_id
          patcaseid              = co_case_id
          movemnt_seqno          = co_movement_number_1
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_1
          transport_type         = co_transport_type_1
        )
    )->set_parameter(
        name          = 'ES_VISIT_DATA_AT'
        value         = VALUE bapi2097atout(
           med_emergency_case     = co_med_emergency_case_1
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_1 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_getoutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
    ).

    " Es sollte eine Bewegung storniert werden, und zwar zuerst einmal im Testmodus,
    " dann mit Verbuchung.
    configure_bapi_call( )->ignore_parameter( 'IS_CANCEL_USER_DATE' )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_2 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_canceloutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
        i_testrun              = abap_true
    ).

    configure_bapi_call( )->ignore_parameter( 'IS_CANCEL_USER_DATE' )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_3 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_canceloutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
        i_testrun              = abap_false
    ).

    " Es sollten weder Bewegungen angelegt noch geändert werden.
    expect_no_addoutpatvisit( ).
    expect_no_changeoutpatvisit( ).

    " Da wir keine Fehler gemeldet haben, sollte ein Commit und kein Rollback erfolgen.
    expect_transaction_handling(
        i_commit   = abap_true
        i_rollback = abap_false
    ).

    " Nachricht verarbeiten
    connector->send(
        ir_logger            = me->logger
        ir_message           = message
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
    ).

    " es darf zu keinem Fehler gekommen sein
    assert_no_error_message( ).
    assert_process_status(
      i_status = if_ishmed_mci_message_constant=>co_message_status_success
    ).

    " alle Test Doubles müssen erwartungsgemäß angesteuert worden sein
    cl_abap_testdouble=>verify_expectations( me->connector->bapi_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->elink_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->history_double ).

    " die Testnachrichten müssen aufgezeichnet worden sein
    assert_test_message_logged(
        i_message_number = co_check_message_number_1
    ).
    assert_test_message_logged(
        i_message_number = co_check_message_number_2
    ).
    assert_test_message_logged(
        i_message_number = co_check_message_number_3
    ).

  ENDMETHOD.

  METHOD cancel_already_cancelled.

    " Eingabenachricht erzeugen und Stornokennzeichen setzen.
    DATA(message) = create_default_message( ).
    message->set_cancelled( abap_true ).

    " Wir erwarten Anfragen bei der History-API, antworten aber mit Initialwerten
    " (weil wir die History hier nicht testen wollen).
    configure_history_response(
        i_message_date = VALUE d( )
        i_message_time = VALUE t( )
    ).

    " Im Rahmen der Verbuchung muss die History-Tabelle aktualisiert werden.
    expect_history_updated( ).

    " Es gibt eine verknüpfte Bewegung.
    configure_elink_response(
        i_objects = VALUE #( ( CONV #( |{ co_institution_id }{ co_case_id }{ co_movement_number_1 }| ) ) )
    ).

    " Die Verknüpfungstabelle wird nicht geändert.
    expect_elink_unchanged( ).

    " Der Ist-Stand der verknüpften Bewegung muss gelesen werden.
    configure_bapi_call( )->set_parameter(
        name          = 'ES_VISIT_DATA'
        value         = VALUE bapi2097ovout(
          client                 = sy-mandt
          institution            = co_institution_id
          patcaseid              = co_case_id
          movemnt_seqno          = co_movement_number_1
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_1
          transport_type         = co_transport_type_1
          cancel_ind             = abap_true " <--- bereits storniert!
        )
    )->set_parameter(
        name          = 'ES_VISIT_DATA_AT'
        value         = VALUE bapi2097atout(
           med_emergency_case     = co_med_emergency_case_1
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_1 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_getoutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
    ).

    " Da die Bewegung bereits storniert ist, soll nichts geändert werden.
    expect_no_addoutpatvisit( ).
    expect_no_changeoutpatvisit( ).
    expect_no_canceloutpatvisit( ).

    " Da wir keine Fehler gemeldet haben, sollte ein Commit und kein Rollback erfolgen.
    expect_transaction_handling(
        i_commit   = abap_true
        i_rollback = abap_false
    ).

    " Nachricht verarbeiten
    connector->send(
        ir_logger            = me->logger
        ir_message           = message
        ir_message_context   = me->context
        ir_runtime_parameter = me->runtime_params
    ).

    " Es darf zu keinem Fehler gekommen sein, aber es sollte eine Warnung aufgezeichnet werden.
    " Die Nachrichtenverarbeitung soll abgebrochen werden.
    assert_no_error_message( ).
    assert_warning_message( ).
    assert_process_status(
      i_status = if_ishmed_mci_message_constant=>co_message_status_abort
    ).

    " alle Test Doubles müssen erwartungsgemäß angesteuert worden sein
    cl_abap_testdouble=>verify_expectations( me->connector->bapi_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->elink_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->history_double ).

    " die Testnachrichten müssen aufgezeichnet worden sein
    assert_test_message_logged(
        i_message_number = co_check_message_number_1
    ).

  ENDMETHOD.

  METHOD error_movement_read.

    " Eingabenachricht erzeugen und Stornokennzeichen setzen.
    DATA(message) = create_default_message( ).
    message->set_cancelled( abap_true ).

    " Wir erwarten Anfragen bei der History-API, antworten aber mit Initialwerten
    " (weil wir die History hier nicht testen wollen).
    configure_history_response(
        i_message_date = VALUE d( )
        i_message_time = VALUE t( )
    ).

    " Aufgrund des Fehlers darf die History-Tabelle nicht aktualisiert werden.
    expect_history_unchanged( ).

    " Es gibt eine verknüpfte Bewegung.
    configure_elink_response(
        i_objects = VALUE #( ( CONV #( |{ co_institution_id }{ co_case_id }{ co_movement_number_1 }| ) ) )
    ).

    " Die Verknüpfungstabelle wird nicht geändert.
    expect_elink_unchanged( ).

    " Der Leseversuch des Ist-Stands der verknüpften Bewegung liefert einen Fehler.
    configure_bapi_call( )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = 'E'
            id     = co_check_message_id
            number = co_check_message_number_1 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = 'E'
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_getoutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
    ).

    " Es sollten weder Bewegungen angelegt noch verändert noch storniert werden.
    expect_no_addoutpatvisit( ).
    expect_no_changeoutpatvisit( ).
    expect_no_canceloutpatvisit( ).

    " Da wir noch nicht in die Verbuchung gegangen sind, sollte weder COMMIT noch
    " ROLLBACK erfolgen.
    expect_transaction_handling(
        i_commit   = abap_false
        i_rollback = abap_false
    ).

    " Nachricht verarbeiten
    TRY.
        connector->send(
            ir_logger            = me->logger
            ir_message           = message
            ir_message_context   = me->context
            ir_runtime_parameter = me->runtime_params
        ).
      CATCH cx_ishmed_mci INTO DATA(mci_exception) ##no_handler.
    ENDTRY.

    " es muss zu einem Fehler gekommen sein, oder der Prozess muss abgebrochen sein
    IF mci_exception IS INITIAL.
      assert_error_message( ).
      assert_process_status(
        i_status = if_ishmed_mci_message_constant=>co_message_status_error
      ).
    ENDIF.

    " alle Test Doubles müssen erwartungsgemäß angesteuert worden sein
    cl_abap_testdouble=>verify_expectations( me->connector->bapi_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->elink_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->history_double ).

    " die Testnachrichten müssen aufgezeichnet worden sein
    assert_test_message_logged(
        i_message_number = co_check_message_number_1
        i_message_type   = 'E'
    ).

  ENDMETHOD.

  METHOD error_test.


    " Eingabenachricht erzeugen und Stornokennzeichen setzen.
    DATA(message) = create_default_message( ).
    message->set_cancelled( abap_true ).

    " Wir erwarten Anfragen bei der History-API, antworten aber mit Initialwerten
    " (weil wir die History hier nicht testen wollen).
    configure_history_response(
        i_message_date = VALUE d( )
        i_message_time = VALUE t( )
    ).

    " Aufgrund des Fehlers darf die History-Tabelle nicht aktualisiert werden.
    expect_history_unchanged( ).

    " Es gibt eine verknüpfte Bewegung.
    configure_elink_response(
        i_objects = VALUE #( ( CONV #( |{ co_institution_id }{ co_case_id }{ co_movement_number_1 }| ) ) )
    ).

    " Die Verknüpfungstabelle wird nicht geändert.
    expect_elink_unchanged( ).

    " Der Ist-Stand der verknüpften Bewegung muss gelesen werden.
    configure_bapi_call( )->set_parameter(
        name          = 'ES_VISIT_DATA'
        value         = VALUE bapi2097ovout(
          client                 = sy-mandt
          institution            = co_institution_id
          patcaseid              = co_case_id
          movemnt_seqno          = co_movement_number_1
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_1
          transport_type         = co_transport_type_1
        )
    )->set_parameter(
        name          = 'ES_VISIT_DATA_AT'
        value         = VALUE bapi2097atout(
           med_emergency_case     = co_med_emergency_case_1
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_1 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_getoutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
    ).

    " Es sollte eine Bewegung storniert werden, und zwar zuerst einmal im Testmodus
    " - da werfen wir schon einen Fehler-
    configure_bapi_call( )->ignore_parameter( 'IS_CANCEL_USER_DATE' )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = 'E'
            id     = co_check_message_id
            number = co_check_message_number_2 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = 'E'
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_canceloutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
        i_testrun              = abap_true
    ).

    " Es sollten weder Bewegungen angelegt noch geändert werden.
    expect_no_addoutpatvisit( ).
    expect_no_changeoutpatvisit( ).

    " Aufgrund des Fehlers erwarten wir ein ROLLBACK.
    expect_transaction_handling(
        i_commit   = abap_false
        i_rollback = abap_true
    ).

    " Nachricht verarbeiten
    TRY.
        connector->send(
            ir_logger            = me->logger
            ir_message           = message
            ir_message_context   = me->context
            ir_runtime_parameter = me->runtime_params
        ).
      CATCH cx_ishmed_mci INTO DATA(mci_exception) ##no_handler.
    ENDTRY.

    " es muss zu einem Fehler gekommen sein, oder der Prozess muss abgebrochen sein
    IF mci_exception IS INITIAL.
      assert_error_message( ).
      assert_process_status(
        i_status = if_ishmed_mci_message_constant=>co_message_status_error
      ).
    ENDIF.

    " alle Test Doubles müssen erwartungsgemäß angesteuert worden sein
    cl_abap_testdouble=>verify_expectations( me->connector->bapi_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->elink_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->history_double ).

    " die Testnachrichten müssen aufgezeichnet worden sein
    assert_test_message_logged(
        i_message_number = co_check_message_number_1
    ).
    assert_test_message_logged(
        i_message_number = co_check_message_number_2
        i_message_type   = 'E'
    ).

  ENDMETHOD.

  METHOD error_update.

    " Eingabenachricht erzeugen und Stornokennzeichen setzen.
    DATA(message) = create_default_message( ).
    message->set_cancelled( abap_true ).

    " Wir erwarten Anfragen bei der History-API, antworten aber mit Initialwerten
    " (weil wir die History hier nicht testen wollen).
    configure_history_response(
        i_message_date = VALUE d( )
        i_message_time = VALUE t( )
    ).

    " Aufgrund des Fehlers darf die History-Tabelle nicht aktualisiert werden.
    expect_history_unchanged( ).

    " Es gibt eine verknüpfte Bewegung.
    configure_elink_response(
        i_objects = VALUE #( ( CONV #( |{ co_institution_id }{ co_case_id }{ co_movement_number_1 }| ) ) )
    ).

    " Die Verknüpfungstabelle wird nicht geändert.
    expect_elink_unchanged( ).

    " Der Ist-Stand der verknüpften Bewegung muss gelesen werden.
    configure_bapi_call( )->set_parameter(
        name          = 'ES_VISIT_DATA'
        value         = VALUE bapi2097ovout(
          client                 = sy-mandt
          institution            = co_institution_id
          patcaseid              = co_case_id
          movemnt_seqno          = co_movement_number_1
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_1
          transport_type         = co_transport_type_1
        )
    )->set_parameter(
        name          = 'ES_VISIT_DATA_AT'
        value         = VALUE bapi2097atout(
           med_emergency_case     = co_med_emergency_case_1
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_1 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_getoutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
    ).

    " Es sollte eine Bewegung storniert werden, und zwar zuerst einmal im Testmodus,
    " dann mit Verbuchung - da werfen wir dann einen Fehler.
    configure_bapi_call( )->ignore_parameter( 'IS_CANCEL_USER_DATE' )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_2 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_canceloutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
        i_testrun              = abap_true
    ).

    configure_bapi_call( )->ignore_parameter( 'IS_CANCEL_USER_DATE' )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = 'E'
            id     = co_check_message_id
            number = co_check_message_number_3 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = 'E'
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_canceloutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
        i_testrun              = abap_false
    ).

    " Es sollten weder Bewegungen angelegt noch geändert werden.
    expect_no_addoutpatvisit( ).
    expect_no_changeoutpatvisit( ).

    " Aufgrund des Fehlers erwarten wir ein ROLLBACK.
    expect_transaction_handling(
        i_commit   = abap_false
        i_rollback = abap_true
    ).

    " Nachricht verarbeiten
    TRY.
        connector->send(
            ir_logger            = me->logger
            ir_message           = message
            ir_message_context   = me->context
            ir_runtime_parameter = me->runtime_params
        ).
      CATCH cx_ishmed_mci INTO DATA(mci_exception) ##no_handler.
    ENDTRY.

    " es muss zu einem Fehler gekommen sein, oder der Prozess muss abgebrochen sein
    IF mci_exception IS INITIAL.
      assert_error_message( ).
      assert_process_status(
        i_status = if_ishmed_mci_message_constant=>co_message_status_error
      ).
    ENDIF.

    " alle Test Doubles müssen erwartungsgemäß angesteuert worden sein
    cl_abap_testdouble=>verify_expectations( me->connector->bapi_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->elink_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->history_double ).

    " die Testnachrichten müssen aufgezeichnet worden sein
    assert_test_message_logged(
        i_message_number = co_check_message_number_1
    ).
    assert_test_message_logged(
        i_message_number = co_check_message_number_2
    ).
    assert_test_message_logged(
        i_message_number = co_check_message_number_3
        i_message_type   = 'E'
    ).

  ENDMETHOD.

  METHOD error_multiple_movements.

    " Eingabenachricht erzeugen und Stornokennzeichen setzen.
    DATA(message) = create_default_message( ).
    message->set_cancelled( abap_true ).

    " Wir erwarten Anfragen bei der History-API, antworten aber mit Initialwerten
    " (weil wir die History hier nicht testen wollen).
    configure_history_response(
        i_message_date = VALUE d( )
        i_message_time = VALUE t( )
    ).

    " Aufgrund des Fehlers darf die History-Tabelle nicht aktualisiert werden.
    expect_history_unchanged( ).

    " Es gibt ZWEI verknüpfte Bewegungen.
    configure_elink_response(
        i_objects = VALUE #(
          ( CONV #( |{ co_institution_id }{ co_case_id }{ co_movement_number_1 }| ) )
          ( CONV #( |{ co_institution_id }{ co_case_id }{ co_movement_number_2 }| ) )
        )
    ).

    " Die Verknüpfungstabelle wird nicht geändert.
    expect_elink_unchanged( ).

    " Der Ist-Stand der verknüpften Bewegungen muss gelesen werden - beide sind
    " aktiv.
    configure_bapi_call( )->set_parameter(
        name          = 'ES_VISIT_DATA'
        value         = VALUE bapi2097ovout(
          client                 = sy-mandt
          institution            = co_institution_id
          patcaseid              = co_case_id
          movemnt_seqno          = co_movement_number_1
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_1
          transport_type         = co_transport_type_1
        )
    )->set_parameter(
        name          = 'ES_VISIT_DATA_AT'
        value         = VALUE bapi2097atout(
           med_emergency_case     = co_med_emergency_case_1
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_1 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_getoutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_1
    ).

    configure_bapi_call( )->set_parameter(
        name          = 'ES_VISIT_DATA'
        value         = VALUE bapi2097ovout(
          client                 = sy-mandt
          institution            = co_institution_id
          patcaseid              = co_case_id
          movemnt_seqno          = co_movement_number_2
          movemnt_type           = co_movement_type_1
          movemnt_date           = co_movement_date_1
          movemnt_time           = co_movement_time_1
          department             = co_department_1
          nurs_treat_ou          = co_ward_2
          transport_type         = co_transport_type_2
        )
    )->set_parameter(
        name          = 'ES_VISIT_DATA_AT'
        value         = VALUE bapi2097atout(
           med_emergency_case     = co_med_emergency_case_2
        )
    )->set_parameter(
        name          = 'ET_MESSAGES'
        value         = VALUE ishmed_t_bapiret2(
          ( type   = co_check_message_type
            id     = co_check_message_id
            number = co_check_message_number_2 )
        )
    )->set_parameter(
        name          = 'E_WORST_RETURNED_MSGTY'
        value         = co_check_message_type
    )->and_expect( )->is_called_once( ).
    me->connector->bapi_double->call_patcase_getoutpatvisit(
      EXPORTING
        i_institution          = co_institution_id
        i_case_id              = co_case_id
        i_movement_number      = co_movement_number_2
    ).

    " Es sollten weder Bewegungen angelegt noch verändert noch storniert werden.
    expect_no_addoutpatvisit( ).
    expect_no_changeoutpatvisit( ).
    expect_no_canceloutpatvisit( ).

    " Da wir noch nicht in die Verbuchung gegangen sind, sollte weder COMMIT noch
    " ROLLBACK erfolgen.
    expect_transaction_handling(
        i_commit   = abap_false
        i_rollback = abap_false
    ).

    " Nachricht verarbeiten
    TRY.
        connector->send(
            ir_logger            = me->logger
            ir_message           = message
            ir_message_context   = me->context
            ir_runtime_parameter = me->runtime_params
        ).
      CATCH cx_ishmed_mci INTO DATA(mci_exception) ##no_handler.
    ENDTRY.

    " es muss zu einem Fehler gekommen sein, oder der Prozess muss abgebrochen sein
    IF mci_exception IS INITIAL.
      assert_error_message( ).
      assert_process_status(
        i_status = if_ishmed_mci_message_constant=>co_message_status_error
      ).
    ENDIF.

    " alle Test Doubles müssen erwartungsgemäß angesteuert worden sein
    cl_abap_testdouble=>verify_expectations( me->connector->bapi_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->elink_double ).
    cl_abap_testdouble=>verify_expectations( me->connector->history_double ).

    " die Testnachrichten müssen aufgezeichnet worden sein
    assert_test_message_logged(
        i_message_number = co_check_message_number_1
    ).
    assert_test_message_logged(
        i_message_number = co_check_message_number_2
    ).

  ENDMETHOD.

ENDCLASS.
