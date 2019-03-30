"! <strong>Beispiel-End-Konnektor Besuchsanlage</strong>
"! <br/>
"! Diese Klasse stellt ein Beispiel zur Implementierung eines End-Konnektors
"! dar.
"! <br/>
"! Diese Klasse ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 8.3
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
CLASS zcl_mcibuch_endconn_visit DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_ishmed_mci_end_connector.

    "! Objektkategorie der Bewegung (BO = BOR-Objekttyp, SWO1)
    CONSTANTS co_object_category TYPE swf_clstyp VALUE 'BO'.

    "! Klasse der Bewegung (BUS1306 = PatientMovement)
    CONSTANTS co_object_type     TYPE sibftypeid VALUE 'BUS1306'.

    METHODS constructor.

  PROTECTED SECTION.
    "! Wrapper zur Ansteuerung der Standard-Bausteine
    DATA bapi_wrapper TYPE REF TO zif_mcibuch_visit_bapi_wrapper .

    "! Erzeugt ein Zugriffsobjekt zur Nutzung der ELINK-API.
    METHODS create_elink_api
      IMPORTING
        i_process_name  TYPE n1mci_process_name
        i_external_key  TYPE n1mci_external_key
      RETURNING
        VALUE(r_result) TYPE REF TO zif_mcibuch_elink_api
      RAISING
        cx_ishmed_mci.

    "! Erzeugt ein Zugriffsobjekt zur Nutzung der HISTORY-API.
    METHODS create_history_api
      IMPORTING
        i_process_name  TYPE n1mci_process_name
        i_external_key  TYPE n1mci_external_key
      RETURNING
        VALUE(r_result) TYPE REF TO zif_mcibuch_history_api
      RAISING
        cx_ishmed_mci.

  PRIVATE SECTION.
    "! Nachrichtenklasse der eigenen Meldungen
    CONSTANTS co_message_id TYPE symsgid VALUE 'ZMCIBUCH'.

    "! Verarbeitet die Nachricht nach Typprüfung.
    METHODS process_visit_message
      IMPORTING
        ir_message           TYPE REF TO zcl_mcibuch_msg_case_visit
        ir_message_context   TYPE REF TO if_ishmed_mci_message_context
        ir_runtime_parameter TYPE REF TO if_ishmed_mci_runtime_params
        ir_logger            TYPE REF TO if_ishmed_mci_logger
      RAISING
        cx_ishmed_mci.

    "! Liest und prüft zu jedem Schlüssel aus der Verknüpfungstabelle
    "! die Bewegung. Stornierte Bewegungen werden gemeldet, aber ansonsten
    "! ignoriert; mehrere zugeordnete aktive Bewegungen deuten auf eine
    "! Inkonsistenz hin und führen zu einem Abbruch mit entsprechender
    "! Fehlermeldung.
    METHODS read_actual_movement
      IMPORTING
        i_institution_id  TYPE einri
        i_case_id         TYPE falnr
        ir_elink_api      TYPE REF TO zif_mcibuch_elink_api
        ir_logger         TYPE REF TO if_ishmed_mci_logger
      EXPORTING
        e_movement_data   TYPE t_movement_data
        e_cancelled_exist TYPE abap_bool
      RAISING
        cx_ishmed_mci.

    METHODS create_movement
      IMPORTING
        ir_message         TYPE REF TO zcl_mcibuch_msg_case_visit
        ir_elink_api       TYPE REF TO zif_mcibuch_elink_api
        ir_message_context TYPE REF TO if_ishmed_mci_message_context
        ir_logger          TYPE REF TO if_ishmed_mci_logger
      RETURNING
        VALUE(r_result)    TYPE abap_bool.

    "! Storno einer Bewegung.
    METHODS cancel_movement
      IMPORTING
        ir_message         TYPE REF TO zcl_mcibuch_msg_case_visit ##needed
        is_actual_movement TYPE t_movement_data
        ir_message_context TYPE REF TO if_ishmed_mci_message_context
        ir_logger          TYPE REF TO if_ishmed_mci_logger
      RETURNING
        VALUE(r_result)    TYPE abap_bool.

    "! Aktualisierung einer Bewegung.
    METHODS update_movement
      IMPORTING
        ir_message         TYPE REF TO zcl_mcibuch_msg_case_visit
        is_actual_movement TYPE t_movement_data
        ir_message_context TYPE REF TO if_ishmed_mci_message_context
        ir_logger          TYPE REF TO if_ishmed_mci_logger
      RETURNING
        VALUE(r_result)    TYPE abap_bool.

    "! Ermittelt die durchzuführenden Änderungen.
    METHODS determine_visit_changes
      IMPORTING
        ir_message                TYPE REF TO zcl_mcibuch_msg_case_visit
        is_actual_movement        TYPE t_movement_data
      EXPORTING
        es_changed_visit_data     TYPE bapi2097ovinx
        es_changed_visit_data_med TYPE bapi2097n1medx
        es_changed_visit_data_at  TYPE bapi2097atinx.

    "! Generische Befüllung einer Änderungsdatenstruktur.
    METHODS fill_change_structure
      IMPORTING
        is_current_data     TYPE data
        is_new_data         TYPE data
        it_excluded_fields  TYPE trfieldnames OPTIONAL
      CHANGING
        cs_change_structure TYPE data.

ENDCLASS.



CLASS ZCL_MCIBUCH_ENDCONN_VISIT IMPLEMENTATION.


  METHOD cancel_movement.

    DATA(action_logger) = NEW cl_ishmed_tpi_action_logger( ir_logger ).

    " Testlauf durchführen
    action_logger->log_action(
        i_loglevel = if_ishmed_mci_log_constant=>co_log_level_info
        i_action   = |BAPI_PATCASE_CANCELOUTPATVISIT (Test)|
    ).
    action_logger->set_timestamp_start( ).
    me->bapi_wrapper->call_patcase_canceloutpatvisit(
      EXPORTING
        i_institution          = is_actual_movement-bapi2097ovout-institution
        i_case_id              = is_actual_movement-bapi2097ovout-patcaseid
        i_movement_number      = is_actual_movement-bapi2097ovout-movemnt_seqno
        i_testrun              = abap_true
      IMPORTING
        e_worst_returned_msgty   = DATA(worst_message_type)
        et_messages              = DATA(messages)
    ).
    action_logger->set_timestamp_end( ).
    action_logger->log_duration(
        i_loglevel = if_ishmed_mci_log_constant=>co_log_level_debug
    ).
    IF messages IS NOT INITIAL.
      cl_ishmed_mci_utl=>log_bapiret_tab(
          ir_logger  = ir_logger
          it_bapiret = messages
      ).
    ELSE.
      " (keine Nachrichten erzeugt)
      ir_logger->info(
          i_msgid    = co_message_id
          i_msgno    = '022'
      ).
    ENDIF.

    IF worst_message_type CA 'EAX'.
      " Fehler im Testlauf, Abbruch
      ir_logger->error(
          i_msgid    = co_message_id
          i_msgno    = '024'
      ).
      ir_message_context->set_status_error( ).
    ELSE.

      FREE: messages, worst_message_type.
      action_logger->clear( ).

      " Verbuchung durchführen
      action_logger->log_action(
          i_loglevel = if_ishmed_mci_log_constant=>co_log_level_info
          i_action   = |BAPI_PATCASE_CANCELOUTPATVISIT|
      ).
      action_logger->set_timestamp_start( ).
      me->bapi_wrapper->call_patcase_canceloutpatvisit(
        EXPORTING
          i_institution          = is_actual_movement-bapi2097ovout-institution
          i_case_id              = is_actual_movement-bapi2097ovout-patcaseid
          i_movement_number      = is_actual_movement-bapi2097ovout-movemnt_seqno
          i_testrun              = abap_false
        IMPORTING
          e_worst_returned_msgty   = worst_message_type
          et_messages              = messages
      ).
      action_logger->set_timestamp_end( ).
      action_logger->log_duration(
          i_loglevel = if_ishmed_mci_log_constant=>co_log_level_debug
      ).
      IF messages IS NOT INITIAL.
        cl_ishmed_mci_utl=>log_bapiret_tab(
            ir_logger  = ir_logger
            it_bapiret = messages
        ).
      ELSE.
        " (keine Nachrichten erzeugt)
        ir_logger->info(
            i_msgid    = co_message_id
            i_msgno    = '022'
        ).
      ENDIF.

      IF worst_message_type CA 'EAX'.
        " Fehler im Verbuchungslauf, Abbruch
        ir_logger->error(
            i_msgid    = co_message_id
            i_msgno    = '025'
        ).
        ir_message_context->set_status_error( ).
      ELSE.
        ir_message_context->set_status_success( ).
        r_result = abap_true.
      ENDIF.

    ENDIF. " keine Fehler im Testlauf

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    me->bapi_wrapper = NEW lcl_bapi_wrapper( ).
  ENDMETHOD.


  METHOD create_elink_api.

    TRY.
        r_result = NEW zcl_mcibuch_wrapper_elink(
            i_process_name    = i_process_name
            i_external_key    = i_external_key
            i_object_category = co_object_category
            i_object_type     = co_object_type
        ).
      CATCH cx_ishmed_locked INTO DATA(locking_error).
        " Der Zugriff auf die Tabelle &TABLE_NAME& ist gesperrt
        RAISE EXCEPTION TYPE zcx_mcibuch_mci_error
          EXPORTING
            textid     = zcx_mcibuch_mci_error=>table_api_locked
            previous   = locking_error
            table_name = 'N1TPI_ELINK'.
    ENDTRY.

  ENDMETHOD.


  METHOD create_history_api.

    TRY.
        r_result = NEW zcl_mcibuch_wrapper_history(
            i_process_name    = i_process_name
            i_external_key    = i_external_key
            i_object_category = co_object_category
            i_object_type     = co_object_type
        ).
      CATCH cx_ishmed_locked INTO DATA(locking_error).
        " Der Zugriff auf die Tabelle &TABLE_NAME& ist gesperrt
        RAISE EXCEPTION TYPE zcx_mcibuch_mci_error
          EXPORTING
            textid     = zcx_mcibuch_mci_error=>table_api_locked
            previous   = locking_error
            table_name = 'N1TPI_HISTORY'.
    ENDTRY.

  ENDMETHOD.


  METHOD create_movement.

    DATA(action_logger) = NEW cl_ishmed_tpi_action_logger( ir_logger ).

    " Daten aus der Nachricht entnehmen
    DATA(institution_id) = ir_message->get_institution( ).
    DATA(case_id)        = ir_message->get_case_id( ).
    DATA(visit_data_adm) = ir_message->get_visit_data_adm( ).
    DATA(visit_data_med) = ir_message->get_visit_data_med( ).
    DATA(visit_data_at)  = ir_message->get_visit_data_at( ).

    " Testlauf durchführen
    action_logger->log_action(
        i_loglevel = if_ishmed_mci_log_constant=>co_log_level_info
        i_action   = |BAPI_PATCASE_ADDOUTPATVISIT (Test)|
    ).
    action_logger->set_timestamp_start( ).
    me->bapi_wrapper->call_patcase_addoutpatvisit(
      EXPORTING
        i_institution            = institution_id
        i_case_id                = case_id
        is_outpat_visit_data     = visit_data_adm
        is_outpat_visit_data_med = visit_data_med
        is_outpat_visit_data_at  = visit_data_at
        i_testrun                = abap_true
      IMPORTING
        e_worst_returned_msgty   = DATA(worst_message_type)
        et_messages              = DATA(messages)
    ).
    action_logger->set_timestamp_end( ).
    action_logger->log_duration(
        i_loglevel = if_ishmed_mci_log_constant=>co_log_level_debug
    ).
    IF messages IS NOT INITIAL.
      cl_ishmed_mci_utl=>log_bapiret_tab(
          ir_logger  = ir_logger
          it_bapiret = messages
      ).
    ELSE.
      " (keine Nachrichten erzeugt)
      ir_logger->info(
          i_msgid    = co_message_id
          i_msgno    = '022'
      ).
    ENDIF.

    IF worst_message_type CA 'EAX'.
      " Fehler im Testlauf, Abbruch
      ir_logger->error(
          i_msgid    = co_message_id
          i_msgno    = '024'
      ).
      ir_message_context->set_status_error( ).
    ELSE.

      FREE: messages, worst_message_type.
      action_logger->clear( ).

      " Verbuchung durchführen
      action_logger->log_action(
          i_loglevel = if_ishmed_mci_log_constant=>co_log_level_info
          i_action   = |BAPI_PATCASE_ADDOUTPATVISIT|
      ).
      action_logger->set_timestamp_start( ).
      me->bapi_wrapper->call_patcase_addoutpatvisit(
        EXPORTING
          i_institution            = institution_id
          i_case_id                = case_id
          is_outpat_visit_data     = visit_data_adm
          is_outpat_visit_data_med = visit_data_med
          is_outpat_visit_data_at  = visit_data_at
          i_testrun                = abap_false
        IMPORTING
          e_worst_returned_msgty   = worst_message_type
          es_new_movement_data     = DATA(new_movement_data)
          et_messages              = messages
      ).
      action_logger->set_timestamp_end( ).
      action_logger->log_duration(
          i_loglevel = if_ishmed_mci_log_constant=>co_log_level_debug
      ).
      IF messages IS NOT INITIAL.
        cl_ishmed_mci_utl=>log_bapiret_tab(
            ir_logger  = ir_logger
            it_bapiret = messages
        ).
      ELSE.
        " (keine Nachrichten erzeugt)
        ir_logger->info(
            i_msgid    = co_message_id
            i_msgno    = '022'
        ).
      ENDIF.

      IF worst_message_type CA 'EAX'.
        " Fehler im Verbuchungslauf, Abbruch
        ir_logger->error(
            i_msgid    = co_message_id
            i_msgno    = '025'
        ).
        ir_message_context->set_status_error( ).
      ELSE.

        " Bewegung dem externen Schlüssel zuordnen
        ir_elink_api->add( CONV #( |{ institution_id }{ case_id }{ new_movement_data-movemnt_seqno }| ) ).
        ir_elink_api->save( ).

        ir_message_context->set_status_success( ).
        r_result = abap_true.
      ENDIF.

    ENDIF. " keine Fehler im Testlauf

  ENDMETHOD.


  METHOD determine_visit_changes.

    " Besonderheit: Die medizinischen Ist-Daten stecken in den "normalen"
    " Bewegungsdaten.
    " Delta-Struktur mit Hilfs-Methode befüllen
    fill_change_structure(
      EXPORTING
        is_current_data     = is_actual_movement-bapi2097ovout
        is_new_data         = ir_message->get_visit_data_adm( )
        it_excluded_fields  = VALUE #(
          ( 'CASETYPE_EXT' )    " Fallart - anwenderspezifisch
          ( 'CASE_COMMENT' )    " Bemerkung zum Fall
          ( 'CASE_CATEGORY' )   " Falltyp
          ( 'FOREIGN_CASE' )    " Auslandsfall
          ( 'NON_RESIDENT' )    " Kennzeichen Patient kein Staatsbürger
          ( 'GEOGR_AREA' )      " Einzugsgebiet
          ( 'EMERG_ADM' )       " Notaufnahmekennzeichen
          ( 'QUICK_ADM' )       " Kurzaufnahmekennzeichen
          ( 'START_DATE' )      " Beginndatum des Falles
          ( 'END_DATE' )        " Endedatum des Falles
          ( 'CASEENDTYPE' )     " Art des Fallendes
          ( 'WORK_INCAPACITY' ) " Arbeitsunfähigkeits-Bis-Datum
          ( 'BILL_BLOCK' )      " Fakturasperre eines Falles
          ( 'STATSTC_BLOCK' )   " Kennzeichen für die Aufnahmeart Statistiksperre
          ( 'PPA_RELEV' )       " Kennzeichen, daß für die KV-Abrechnung relevant
          ( 'REC_ORDER' )       " Empfänger (Auftrag)
          ( 'CHILDREN' )        " Anzahl Kinder
          ( 'EMPLOYEE_TYPE' )   " Arbeitnehmertyp
          ( 'CANTON_TARIFF' )   " Kantonstarif
          ( 'CANTON_CONVTN' )   " Abkommen (Spital <-> Kanton)
          ( 'APPLSTATUS' )      " Applikationsstatus
          ( 'SPECIALTY' )       " Fachrichtung für Organisationseinheit
          ( 'UPDATE_DATE' )     " Änderungsdatum
          ( 'UPDATE_USER' )     " Name des ändernden Sachbearbeiters
        )
      CHANGING
        cs_change_structure = es_changed_visit_data
    ).

    fill_change_structure(
      EXPORTING
        is_current_data     = CORRESPONDING bapi2097n1med(
                                is_actual_movement-bapi2097ovout ) ##enh_ok
        is_new_data         = ir_message->get_visit_data_med( )
      CHANGING
        cs_change_structure = es_changed_visit_data_med
    ).

    fill_change_structure(
      EXPORTING
        is_current_data     = is_actual_movement-bapi2097atout
        is_new_data         = ir_message->get_visit_data_at( )
      CHANGING
        cs_change_structure = es_changed_visit_data_at
    ).

  ENDMETHOD.


  METHOD fill_change_structure.

    " Range-Tabelle für auszuschließende Felder aufbauen
    DATA excluded_fields TYPE RANGE OF fieldname.
    excluded_fields = VALUE #(
      FOR field IN it_excluded_fields
      ( sign   = 'I'
        option = 'EQ'
        low    = field )
    ).

    " Ziel-Struktur feldweise durchgehen; dabei ausgeschlossene Felder und die
    " X-Felder übergehen
    DATA(descriptor) = CAST cl_abap_structdescr(
      cl_abap_typedescr=>describe_by_data( cs_change_structure )
    ).
    DATA(components) = descriptor->get_components( ).
    LOOP AT components ASSIGNING FIELD-SYMBOL(<component>)
      WHERE type->absolute_name <> '\TYPE=BAPIUPDATE'.

      " excluded_fields kann leer sein - in der WHERE-Klausel oben würden dann
      " alle Felder ausgeschlossen, deshalb separate Prüfung
      IF excluded_fields IS NOT INITIAL.
        CHECK <component>-name NOT IN excluded_fields.
      ENDIF.

      " Quell- und Zieldfelder zuweisen
      DATA(flag_field_name) = |{ <component>-name }X|.
      ASSIGN COMPONENT <component>-name OF STRUCTURE is_current_data
        TO FIELD-SYMBOL(<current_value>).
      ASSIGN COMPONENT <component>-name OF STRUCTURE is_new_data
        TO FIELD-SYMBOL(<new_value>).
      ASSIGN COMPONENT <component>-name OF STRUCTURE cs_change_structure
        TO FIELD-SYMBOL(<changed_value>).
      ASSIGN COMPONENT flag_field_name OF STRUCTURE cs_change_structure
        TO FIELD-SYMBOL(<changed_flag>).

      " Zuweisung und Inhalt prüfen und ggf. übertragen
      IF <current_value> IS ASSIGNED AND
         <new_value>     IS ASSIGNED AND
         <changed_value> IS ASSIGNED AND
         <changed_flag>  IS ASSIGNED.
        IF <current_value> <> <new_value>.
          <changed_value> = <new_value>.
          <changed_flag> = abap_true.
        ENDIF.
      ENDIF.

      UNASSIGN: <current_value>, <new_value>, <changed_value>, <changed_flag>.
    ENDLOOP.

  ENDMETHOD.


  METHOD if_ishmed_mci_component~init ##needed.
  ENDMETHOD.


  METHOD if_ishmed_mci_end_connector~send.

    " MCI-Nachrichtentyp prüfen - wir erwarten eine Besuchsanlage-Nachricht
    "   Hinweis: Ab Basis-Release 7.50 geht das einfacher:
    "   IF cr_message IS INSTANCE OF zcl_mcibuch_msg_case_visit.
    DATA(visit_message_descriptor) = CAST cl_abap_classdescr(
        cl_abap_typedescr=>describe_by_name( 'ZCL_MCIBUCH_MSG_CASE_VISIT' )
    ).
    IF visit_message_descriptor->applies_to( ir_message ).
      process_visit_message(
          ir_message           = CAST zcl_mcibuch_msg_case_visit( ir_message )
          ir_message_context   = ir_message_context
          ir_runtime_parameter = ir_runtime_parameter
          ir_logger            = ir_logger
      ).
    ELSE.
      " Die übergebene Nachricht ist keine Nachricht zur Besuchsanlage
      RAISE EXCEPTION TYPE zcx_mcibuch_mci_error
        EXPORTING
          textid = zcx_mcibuch_mci_error=>message_type_not_addvisit.
    ENDIF.

  ENDMETHOD.


  METHOD if_ishmed_object~finalize ##needed.
  ENDMETHOD.


  METHOD process_visit_message.

    " prüfen, ob wir die relevanten Kontextdaten haben
    DATA(institution_id) = ir_message_context->get_einri( ).
    DATA(case_id) = ir_message_context->get_falnr( ).
    IF institution_id IS INITIAL OR case_id IS INITIAL.
      " Nachrichtenkontext enthält keine Einrichtung und/oder keine Fallnummer
      RAISE EXCEPTION TYPE zcx_mcibuch_mci_error
        EXPORTING
          textid = zcx_mcibuch_mci_error=>institution_or_case_missing.
    ENDIF.
    DATA(external_key) = ir_message_context->get_external_key( ).
    IF external_key IS INITIAL.
      " Der Nachrichtenkontext enthält keinen externen Schlüssel
      RAISE EXCEPTION TYPE zcx_mcibuch_mci_error
        EXPORTING
          textid = zcx_mcibuch_mci_error=>external_key_missing.
    ENDIF.
    DATA(message_date) = ir_message_context->get_message_date( ).
    DATA(message_time) = ir_message_context->get_message_time( ).
    IF message_date IS INITIAL.
      " Der Nachrichtenkontext enthält keine Zeitangabe
      RAISE EXCEPTION TYPE zcx_mcibuch_mci_error
        EXPORTING
          textid = zcx_mcibuch_mci_error=>date_time_missing.
    ENDIF.

    TRY.

        " prüfen, ob die Nachricht veraltet ist
        DATA(history_api) = create_history_api(
            i_process_name = ir_runtime_parameter->get_process_name( )
            i_external_key = external_key
        ).
        IF ( history_api->get_message_date( ) > message_date ) OR
           ( history_api->get_message_date( ) = message_date AND
             history_api->get_message_time( ) > message_time ).
          " Nachricht ist veraltet und wird nicht verarbeitet
          ir_logger->warn(
              i_msgid    = co_message_id
              i_msgno    = '031'
          ).
          ir_message_context->set_status_abort( ).
        ELSE.

          " prüfen, ob wir zu diesem externen Schlüssel schon einmal eine Bewegung
          " angelegt haben und die dazu gehörigen Bewegungsdaten lesen
          DATA(elink_api) = create_elink_api(
              i_process_name = ir_runtime_parameter->get_process_name( )
              i_external_key = external_key
          ).
          read_actual_movement(
            EXPORTING
              i_institution_id  = institution_id
              i_case_id         = case_id
              ir_elink_api      = elink_api
              ir_logger         = ir_logger
            IMPORTING
              e_movement_data   = DATA(actual_movement)
              e_cancelled_exist = DATA(cancelled_movements_exist)
          ).

          " entscheiden, welche Aktion erforderlich ist
          IF ir_message->is_cancelled( ) = abap_true.
            " Zielzustand: stornierte Bewegung

            IF cancelled_movements_exist = abap_true.
              " Bewegung ist bereits storniert
              ir_logger->warn(
                  i_msgid    = co_message_id
                  i_msgno    = '037'
              ).
              ir_message_context->set_status_abort( ).
              DATA(message_processed) = abap_true.
            ELSEIF actual_movement IS INITIAL.
              " es gibt weder eine stornierte noch eine aktive Bewegung
              " Neuanlage einer stornierten Bewegung wird nicht unterstützt
              ir_logger->warn(
                  i_msgid    = co_message_id
                  i_msgno    = '036'
              ).
              ir_message_context->set_status_abort( ).
              message_processed = abap_true.
            ELSE.
              " es gibt eine Bewegung, die storniert werden soll.
              message_processed = cancel_movement(
                  ir_message         = ir_message
                  is_actual_movement = actual_movement
                  ir_message_context = ir_message_context
                  ir_logger          = ir_logger
               ).
            ENDIF.

          ELSE. " ir_message->is_cancelled( ) = abap_true
            " Zielzustand: aktive Bewegung

            " Wenn es bereits stornierte Bewegungen gibt, ignorieren wir diese
            " und legen ggf. eine neue Bewegung an.

            IF actual_movement IS INITIAL.
              message_processed = create_movement(
                  ir_message         = ir_message
                  ir_message_context = ir_message_context
                  ir_elink_api       = elink_api
                  ir_logger          = ir_logger
              ).
            ELSE.
              message_processed = update_movement(
                  ir_message         = ir_message
                  is_actual_movement = actual_movement
                  ir_message_context = ir_message_context
                  ir_logger          = ir_logger
               ).
            ENDIF.
          ENDIF.

          " Zeitstempel sichern und verbuchen
          IF message_processed = abap_true.
            history_api->set_message_date( message_date ).
            history_api->set_message_time( message_time ).
            history_api->save( ).
            me->bapi_wrapper->call_transaction_commit(
              EXPORTING
                i_wait     = abap_true
              IMPORTING
                es_message = DATA(message)
            ).
          ELSE.
            me->bapi_wrapper->call_transaction_rollback(
              IMPORTING
                es_message = message
            ).
          ENDIF.
          IF message IS NOT INITIAL.
            cl_ishmed_mci_utl=>log_bapiret(
                ir_logger  = ir_logger
                is_message = message
            ).
          ENDIF.


          " Sperren abbauen (1)
          TRY.
              elink_api->if_ishmed_object~finalize( ).
            CATCH cx_ishmed_object ##no_handler.
          ENDTRY.
          FREE elink_api.

        ENDIF. " Nachricht nicht veraltet

        " Sperren abbauen (2)
        TRY.
            history_api->if_ishmed_object~finalize( ).
          CATCH cx_ishmed_object ##no_handler.
        ENDTRY.
        FREE history_api.

      CLEANUP.
        " im Fehlerfall noch eventuell sperrende Objekte aufräumen
        IF history_api IS NOT INITIAL.
          TRY.
              history_api->if_ishmed_object~finalize( ).
            CATCH cx_ishmed_object ##no_handler.
          ENDTRY.
          FREE history_api.
        ENDIF.
        IF elink_api IS NOT INITIAL.
          TRY.
              elink_api->if_ishmed_object~finalize( ).
            CATCH cx_ishmed_object ##no_handler.
          ENDTRY.
          FREE elink_api.
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD read_actual_movement.

    DATA visit_data TYPE bapi2097ovout.
    DATA visit_data_at TYPE bapi2097atout.
    DATA worst_message_type TYPE ish_bapiretmaxty.
    DATA messages TYPE ishmed_t_bapiret2.

    FREE: e_cancelled_exist, e_movement_data.

    DATA(action_logger) = NEW cl_ishmed_tpi_action_logger( ir_logger ).

    " alle abgelegten Objektschlüssel durchgehen
    DATA(object_ids) = ir_elink_api->get_object_id_t( ).
    LOOP AT object_ids ASSIGNING FIELD-SYMBOL(<object_id>).
      " object_id = EEEEFFFFFFFFFFBBBBB (Einrichtung-Fall-Bewegung)
      "            +012345678901234
      DATA(movement_number) = CONV lfdbew( <object_id>+14 ).

      " nachsehen, ob wir eine Bewegung mit diesem Schlüssel finden
      action_logger->log_action(
          i_loglevel = if_ishmed_mci_log_constant=>co_log_level_info
          i_action   = |BAPI_PATCASE_ADDOUTPATVISIT[{ movement_number }]|
      ).
      FREE: visit_data, visit_data_at, worst_message_type, messages.
      action_logger->set_timestamp_start( ).
      me->bapi_wrapper->call_patcase_getoutpatvisit(
        EXPORTING
          i_institution          = i_institution_id
          i_case_id              = i_case_id
          i_movement_number      = movement_number
        IMPORTING
          es_visit_data          = visit_data
          es_visit_data_at       = visit_data_at
          e_worst_returned_msgty = worst_message_type
          et_messages            = messages
      ).
      action_logger->set_timestamp_end( ).
      action_logger->log_duration(
          i_loglevel = if_ishmed_mci_log_constant=>co_log_level_debug
      ).
      IF messages IS NOT INITIAL.
        cl_ishmed_mci_utl=>log_bapiret_tab(
            ir_logger  = ir_logger
            it_bapiret = messages
        ).
      ELSE.
        " (keine Nachrichten erzeugt)
        ir_logger->info(
            i_msgid    = co_message_id
            i_msgno    = '022'
        ).
      ENDIF.
      IF worst_message_type CA 'EAX'.
        " Fehler beim Zugriff auf Bewegung &MOVEMENT_NUMBER& des Falls &CASE_ID&
        " in Einrichtung &INSTITUTION_ID&
        RAISE EXCEPTION TYPE zcx_mcibuch_mci_error
          EXPORTING
            textid          = zcx_mcibuch_mci_error=>movement_read_error
            institution_id  = i_institution_id
            case_id         = i_case_id
            movement_number = movement_number.
      ELSE.

        " gültige Bewegung gefunden - ist sie storniert?
        IF visit_data-cancel_ind = abap_true.
          " Stornierte Bewegung &1 wird ignoriert
          ir_logger->info(
              i_msgid    = co_message_id
              i_msgno    = '034'
              i_msgv1 = movement_number
          ).
          e_cancelled_exist = abap_true.
        ELSE.
          " haben wir vorher schon einmal eine Bewegung gefunden?
          IF e_movement_data IS NOT INITIAL.
            " Inkonsistenz: Mehrere Bewegungen zum externen Schlüssel
            " &EXTERNAL_KEY& gefunden
            RAISE EXCEPTION TYPE zcx_mcibuch_mci_error
              EXPORTING
                textid       = zcx_mcibuch_mci_error=>movement_data_inconsistent
                external_key = ir_elink_api->get_external_key( ).
          ENDIF.
          " Bewegungsdaten zur Übergabe merken, aber weitersuchen, um
          " eventuelle Inkonsistenzen zu finden
          e_movement_data-bapi2097ovout = visit_data.
          e_movement_data-bapi2097atout = visit_data_at.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD update_movement.

    DATA(action_logger) = NEW cl_ishmed_tpi_action_logger( ir_logger ).

    " Daten aus der Nachricht entnehmen
    DATA(institution_id) = ir_message->get_institution( ).
    DATA(case_id)        = ir_message->get_case_id( ).

    " Änderungsstrukturen aufbauen
    determine_visit_changes(
      EXPORTING
        ir_message                = ir_message
        is_actual_movement        = is_actual_movement
      IMPORTING
        es_changed_visit_data     = DATA(changed_visit_data)
        es_changed_visit_data_med = DATA(changed_visit_data_med)
        es_changed_visit_data_at  = DATA(changed_visit_data_at)
    ).

    " Testlauf durchführen
    action_logger->log_action(
        i_loglevel = if_ishmed_mci_log_constant=>co_log_level_info
        i_action   = |BAPI_PATCASE_CHANGEOUTPATVISIT (Test)|
    ).
    action_logger->set_timestamp_start( ).
    me->bapi_wrapper->call_patcase_changeoutpatvisit(
      EXPORTING
        i_institution             = institution_id
        i_case_id                 = case_id
        i_movement_number         = is_actual_movement-bapi2097ovout-movemnt_seqno
        is_changed_visit_data     = changed_visit_data
        is_changed_visit_data_med = changed_visit_data_med
        is_changed_visit_data_at  = changed_visit_data_at
        i_testrun                 = abap_true
      IMPORTING
        e_worst_returned_msgty    = DATA(worst_message_type)
        et_messages               = DATA(messages)
    ).
    action_logger->set_timestamp_end( ).
    action_logger->log_duration(
        i_loglevel = if_ishmed_mci_log_constant=>co_log_level_debug
    ).
    IF messages IS NOT INITIAL.
      cl_ishmed_mci_utl=>log_bapiret_tab(
          ir_logger  = ir_logger
          it_bapiret = messages
      ).
    ELSE.
      " (keine Nachrichten erzeugt)
      ir_logger->info(
          i_msgid    = co_message_id
          i_msgno    = '022'
      ).
    ENDIF.

    IF worst_message_type CA 'EAX'.
      " Fehler im Testlauf, Abbruch
      ir_logger->error(
          i_msgid    = co_message_id
          i_msgno    = '024'
      ).
      ir_message_context->set_status_error( ).
    ELSE.

      FREE: messages, worst_message_type.
      action_logger->clear( ).

      " Verbuchung durchführen
      action_logger->log_action(
          i_loglevel = if_ishmed_mci_log_constant=>co_log_level_info
          i_action   = |BAPI_PATCASE_CHANGEOUTPATVISIT|
      ).
      action_logger->set_timestamp_start( ).
      me->bapi_wrapper->call_patcase_changeoutpatvisit(
        EXPORTING
          i_institution             = institution_id
          i_case_id                 = case_id
          i_movement_number         = is_actual_movement-bapi2097ovout-movemnt_seqno
          is_changed_visit_data     = changed_visit_data
          is_changed_visit_data_med = changed_visit_data_med
          is_changed_visit_data_at  = changed_visit_data_at
          i_testrun                 = abap_false
        IMPORTING
          e_worst_returned_msgty   = worst_message_type
          et_messages              = messages
        ).
      action_logger->set_timestamp_end( ).
      action_logger->log_duration(
          i_loglevel = if_ishmed_mci_log_constant=>co_log_level_debug
      ).
      IF messages IS NOT INITIAL.
        cl_ishmed_mci_utl=>log_bapiret_tab(
            ir_logger  = ir_logger
            it_bapiret = messages
        ).
      ELSE.
        " (keine Nachrichten erzeugt)
        ir_logger->info(
            i_msgid    = co_message_id
            i_msgno    = '022'
        ).
      ENDIF.

      IF worst_message_type CA 'EAX'.
        " Fehler im Verbuchungslauf, Abbruch
        ir_logger->error(
            i_msgid    = co_message_id
            i_msgno    = '025'
        ).
        ir_message_context->set_status_error( ).
      ELSE.
        ir_message_context->set_status_success( ).
        r_result = abap_true.
      ENDIF.

    ENDIF. " keine Fehler im Testlauf

  ENDMETHOD.
ENDCLASS.
