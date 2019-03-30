**
** Mockup file access implementation
**
CLASS lcl_mock_file_access DEFINITION FINAL.

  PUBLIC SECTION.

    INTERFACES zif_mcibuch_file_access.

    "! Initialzustand, keine Operation unternommen
    CONSTANTS phase_initial TYPE i VALUE 0.

    "! Datei wurde erfolgreich geöffnet
    CONSTANTS phase_opened  TYPE i VALUE 1.

    "! Mindestens eine Zeile wurde gelesen, es folgen weitere Zeilen
    CONSTANTS phase_reading TYPE i VALUE 2.

    "! Dateiende erreicht
    CONSTANTS phase_eof     TYPE i VALUE 3.

    "! Datei wurde geschlossen
    CONSTANTS phase_closed  TYPE i VALUE 4.

    "! Datei wurde gelöscht
    CONSTANTS phase_deleted TYPE i VALUE 5.

    "! Der aktuelle Zustand des Konnektors. Mit dieser Variablen wird die korrekte
    "! Reihenfolge der Aufrufe kontrolliert.
    DATA current_phase TYPE i VALUE phase_initial.

    "! Test-Steuerung: Simulation einer existiernden/fehlenden Datei beim Öffnen.
    DATA file_exists TYPE abap_bool VALUE abap_true.

    "! Zu liefernde Datei-Inhalte.
    DATA file_contents TYPE stringtab.

    "! Kennzeichen, dass die Datei simuliert gelöscht wurde.
    DATA file_deleted TYPE abap_bool VALUE abap_false.

  PROTECTED SECTION.

    "! Nächste zu liefernde Zeile der simulierten Inhalte.
    DATA next_line TYPE i.

ENDCLASS.

CLASS lcl_mock_file_access IMPLEMENTATION.

  METHOD zif_mcibuch_file_access~open_dataset.

    " Konnektor muss im Initialzustand sein
    cl_abap_unit_assert=>assert_equals(
        act = me->current_phase
        exp = phase_initial
        msg = |OPEN DATASET attempted in invalid phase { me->current_phase }|
    ).

    " existiert die simulierte Datei?
    IF me->file_exists = abap_true.
      " ja - Phasenübergang und alles OK
      me->current_phase = phase_opened.
      me->next_line = 1.
    ELSE.
      " nein - Ausnahme werfen
      RAISE EXCEPTION TYPE zcx_mcibuch_file_not_found.
    ENDIF.

  ENDMETHOD.

  METHOD zif_mcibuch_file_access~read_dataset.

    " Status des Konnektors prüfen.
    CASE me->current_phase.
      WHEN phase_opened.
        " OK - direkt nach dem Öffnen der Datei gerufen
      WHEN phase_reading.
        " OK - Folgeaufruf nach Lesezugriff
      WHEN OTHERS.
        cl_abap_unit_assert=>fail(
            msg = |READ DATASET attempted in invalid phase { me->current_phase }|
        ).
    ENDCASE.

    " Ende der Datei erreicht?
    IF me->next_line > lines( me->file_contents ).
      " ja - Phasenübergang und Ausnahme werfen
      me->current_phase = phase_eof.
      RAISE EXCEPTION TYPE zcx_mcibuch_eof_reached.
    ELSE.
      " nein - Ergebnis liefern
      r_result = me->file_contents[ me->next_line ].
      me->current_phase = phase_reading.
      me->next_line = me->next_line + 1.
    ENDIF.

  ENDMETHOD.

  METHOD zif_mcibuch_file_access~close_dataset.

    " Konnektor muss Dateiende erreicht haben
    cl_abap_unit_assert=>assert_equals(
        act = me->current_phase
        exp = phase_eof
        msg = |CLOSE DATASET attempted in invalid phase { me->current_phase }|
    ).

    " Phasenübergang und OK
    me->current_phase = phase_closed.

  ENDMETHOD.

  METHOD zif_mcibuch_file_access~delete_dataset.

    " nur simuliert existierende Dateien dürfen simuliert gelöscht werden
    cl_abap_unit_assert=>assert_true(
        act = me->file_exists
        msg = |Attempt to delete non-existing file|
    ).

    " Konnektor muss Datei wieder geschlossen haben
    cl_abap_unit_assert=>assert_equals(
        act = me->current_phase
        exp = phase_closed
        msg = |DELETE DATASET attempted in invalid phase { me->current_phase }|
    ).

    " Status setzen, Phasenübergang und OK
    me->file_deleted = abap_true.
    me->current_phase = phase_deleted.

  ENDMETHOD.

ENDCLASS.

**
** Local Subclass with Test Stubs
**
CLASS lcl_startconn DEFINITION FINAL
  INHERITING FROM zcl_mcibuch_startconn_csv_file.

  PUBLIC SECTION.
    "! Zugriff auf die Mockup-Klasse für Dateioperationen gewähren
    DATA mock_file_access TYPE REF TO lcl_mock_file_access READ-ONLY.

  PROTECTED SECTION.
    METHODS initialize_file_access REDEFINITION.


ENDCLASS.

CLASS lcl_startconn IMPLEMENTATION.

  METHOD initialize_file_access.
    CREATE OBJECT me->mock_file_access.
    r_result = me->mock_file_access.
  ENDMETHOD.

ENDCLASS.

**
** Actual Unit Test
**
CLASS lcl_unit_test DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA errorhandler TYPE REF TO cl_ishmed_errorhandling.
    DATA logger TYPE REF TO if_ishmed_mci_logger.
    DATA start_connector TYPE REF TO lcl_startconn.

    DATA published_messages TYPE STANDARD TABLE
                              OF REF TO zcl_mcibuch_msg_csv_dataset.

    METHODS setup
      RAISING
        cx_ishmed_mci.
    METHODS teardown
      RAISING
        cx_ishmed_object.

    METHODS assert_error_message ##relax.
    METHODS assert_no_error_message.
    METHODS assert_warning_message.

    METHODS handle_message_published
          FOR EVENT publish OF if_ishmed_mci_start_connector
      IMPORTING
          ir_message.

    METHODS file_not_exists FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    METHODS file_empty FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    METHODS file_header_only FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    METHODS file_single_line FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    METHODS file_multiple_lines FOR TESTING
      RAISING
        cx_ishmed_object
        cx_ishmed_mci.

    METHODS restart FOR TESTING
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
    CREATE OBJECT me->start_connector.
    me->start_connector->if_ishmed_mci_start_connector~init(
        i_type             = if_ishmed_mci_comp_constant=>co_start_connector
        ir_logger          = me->logger
    ).
    SET HANDLER me->handle_message_published FOR me->start_connector.
  ENDMETHOD.

  METHOD teardown.
    me->start_connector->if_ishmed_mci_start_connector~finalize( ).
    FREE me->start_connector.
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
      cl_abap_unit_assert=>fail( msg = 'Expected error message not logged' ).
    ENDIF.
  ENDMETHOD.

  METHOD assert_no_error_message.
    me->errorhandler->get_max_errortype(
      IMPORTING
        e_maxty         = DATA(worst_message_type)
    ).
    IF worst_message_type CA 'EAX'.
      cl_abap_unit_assert=>fail( msg = 'Unexpected error message logged' ).
    ENDIF.
  ENDMETHOD.

  METHOD assert_warning_message.
    me->errorhandler->get_max_errortype(
      IMPORTING
        e_maxty         = DATA(worst_message_type)
    ).
    IF worst_message_type <> 'W'.
      cl_abap_unit_assert=>fail(
          msg = 'Unexpected error message logged, or expected warning missing'
      ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_message_published.
    TRY.
        DATA(csv_message) = CAST zcl_mcibuch_msg_csv_dataset( ir_message ).
        APPEND csv_message TO me->published_messages.
      CATCH cx_sy_move_cast_error INTO DATA(cast_error).
        cl_abap_unit_assert=>fail(
            msg    = |Published message object is of wrong type|
            detail = cast_error->get_text( )
        ).
    ENDTRY.
  ENDMETHOD.

  METHOD file_not_exists.

    " fehlende Datei simulieren
    me->start_connector->mock_file_access->file_exists = abap_false.

    " Nachrichtenempfang starten
    me->start_connector->if_ishmed_mci_start_connector~receive(
        ir_logger = me->logger
    ).

    " eine fehlende Datei darf keine Fehlernachricht hervorrufen, das ist normal
    assert_no_error_message( ).

    " es dürfen keine Nachrichten erzeugt worden sein
    cl_abap_unit_assert=>assert_initial(
        act = me->published_messages
        msg = |Unexpected messages published|
    ).

  ENDMETHOD.

  METHOD file_empty.

    " Nachrichtenempfang starten
    me->start_connector->if_ishmed_mci_start_connector~receive(
        ir_logger = me->logger
    ).

    " eine leere Datei soll eine Warnung erzeugen
    assert_warning_message( ).

    " es dürfen keine Nachrichten erzeugt worden sein
    cl_abap_unit_assert=>assert_initial(
        act = me->published_messages
        msg = |Unexpected messages published|
    ).

    " die Datei muss gelöscht worden sein
    cl_abap_unit_assert=>assert_true(
        act = me->start_connector->mock_file_access->file_deleted
        msg = |Input file was not deleted|
    ).

  ENDMETHOD.

  METHOD file_header_only.

    " Eingabedaten bereitstellen
    me->start_connector->mock_file_access->file_contents = VALUE #(
      ( `FOO;BAR;BAZ` )
    ).

    " Nachrichtenempfang starten
    me->start_connector->if_ishmed_mci_start_connector~receive(
        ir_logger = me->logger
    ).

    " eine quasi-leere Datei soll eine Warnung erzeugen
    assert_warning_message( ).

    " es dürfen keine Nachrichten erzeugt worden sein
    cl_abap_unit_assert=>assert_initial(
        act = me->published_messages
        msg = |Unexpected messages published|
    ).

    " die Datei muss gelöscht worden sein
    cl_abap_unit_assert=>assert_true(
        act = me->start_connector->mock_file_access->file_deleted
        msg = |Input file was not deleted|
    ).

  ENDMETHOD.

  METHOD file_single_line.

    " Eingabedaten bereitstellen
    me->start_connector->mock_file_access->file_contents = VALUE #(
      ( `FOO;BAR;BAZ` )
      ( `1;one;ett` )
    ).

    " Nachrichtenempfang starten
    me->start_connector->if_ishmed_mci_start_connector~receive(
        ir_logger = me->logger
    ).

    " es darf kein Fehler auftreten
    assert_no_error_message( ).

    " es muss eine Nachricht erzeugt worden sein
    cl_abap_unit_assert=>assert_equals(
        act = lines( me->published_messages )
        exp = 1
        msg = |Unexpected number of messages published|
    ).

    " Inhalt der ersten Nachricht prüfen
    DATA(message_1) = me->published_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals(
        act = message_1->get_line( )
        exp = 1
        msg = |Wrong line number of message 1|
    ).
    message_1->get_data(
      IMPORTING
        e_value = DATA(message_1_contents)
    ).
    cl_abap_unit_assert=>assert_equals(
        act = message_1_contents
        exp = |FOO;BAR;BAZ{ cl_abap_char_utilities=>cr_lf }1;one;ett|
        msg = |Wrong contents of message 1|
    ).

    " die Datei muss gelöscht worden sein
    cl_abap_unit_assert=>assert_true(
        act = me->start_connector->mock_file_access->file_deleted
        msg = |Input file was not deleted|
    ).

  ENDMETHOD.

  METHOD file_multiple_lines.

    " Eingabedaten bereitstellen
    me->start_connector->mock_file_access->file_contents = VALUE #(
      ( `FOO;BAR;BAZ` )
      ( `1;one;ett` )
      ( `2;two;två` )
      ( `4;four;fyra` )
    ).

    " Nachrichtenempfang starten
    me->start_connector->if_ishmed_mci_start_connector~receive(
        ir_logger = me->logger
    ).

    " es darf kein Fehler auftreten
    assert_no_error_message( ).

    " es müssen drei Nachrichten erzeugt worden sein
    cl_abap_unit_assert=>assert_equals(
        act = lines( me->published_messages )
        exp = 3
        msg = |Unexpected number of messages published|
    ).

    " Inhalt der ersten Nachricht prüfen
    DATA(message_1) = me->published_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals(
        act = message_1->get_line( )
        exp = 1
        msg = |Wrong line number of message 1|
    ).
    message_1->get_data(
      IMPORTING
        e_value = DATA(message_1_contents)
    ).
    cl_abap_unit_assert=>assert_equals(
        act = message_1_contents
        exp = |FOO;BAR;BAZ{ cl_abap_char_utilities=>cr_lf }1;one;ett|
        msg = |Wrong contents of message 1|
    ).

    " Inhalt der zweiten Nachricht prüfen
    DATA(message_2) = me->published_messages[ 2 ].
    cl_abap_unit_assert=>assert_equals(
        act = message_2->get_line( )
        exp = 2
        msg = |Wrong line number of message 2|
    ).
    message_2->get_data(
      IMPORTING
        e_value = DATA(message_2_contents)
    ).
    cl_abap_unit_assert=>assert_equals(
        act = message_2_contents
        exp = |FOO;BAR;BAZ{ cl_abap_char_utilities=>cr_lf }2;two;två|
        msg = |Wrong contents of message 2|
    ).

    " Inhalt der dritten Nachricht prüfen
    DATA(message_3) = me->published_messages[ 3 ].
    cl_abap_unit_assert=>assert_equals(
        act = message_3->get_line( )
        exp = 3
        msg = |Wrong line number of message 3|
    ).
    message_3->get_data(
      IMPORTING
        e_value = DATA(message_3_contents)
    ).
    cl_abap_unit_assert=>assert_equals(
        act = message_3_contents
        exp = |FOO;BAR;BAZ{ cl_abap_char_utilities=>cr_lf }4;four;fyra|
        msg = |Wrong contents of message 3|
    ).

    " die Datei muss gelöscht worden sein
    cl_abap_unit_assert=>assert_true(
        act = me->start_connector->mock_file_access->file_deleted
        msg = |Input file was not deleted|
    ).

  ENDMETHOD.

  METHOD restart.

    " Hier wird nur getestet, dass der Start-Konnektor die Testnachricht
    " unverändert re-emittiert.

    " Testnachricht erstellen
    DATA(message_in) = NEW zcl_mcibuch_msg_csv_dataset(
        is_file = VALUE #( )
        i_line  = 42
        i_data  = `foo;bar;baz`
    ).

    " Nachrichtenverarbeitung erneut starten
    me->start_connector->if_ishmed_mci_start_connector~restart(
        ir_logger  = me->logger
        ir_message = message_in
    ).

    " es darf kein Fehler auftreten
    assert_no_error_message( ).

    " es muss eine Nachricht erzeugt worden sein
    cl_abap_unit_assert=>assert_equals(
        act = lines( me->published_messages )
        exp = 1
        msg = |Unexpected number of messages published|
    ).

    " die Nachricht muss identisch mit der Eingabenachricht sein
    cl_abap_unit_assert=>assert_equals(
        act = me->published_messages[ 1 ]
        exp = message_in
        msg = |Modified or entirely different message published|
    ).

    " es darf kein Dateizugriff stattgefunden haben
    cl_abap_unit_assert=>assert_equals(
        act = me->start_connector->mock_file_access->current_phase
        exp = me->start_connector->mock_file_access->phase_initial
        msg = |Unexpected file access detected|
    ).

  ENDMETHOD.

ENDCLASS.
