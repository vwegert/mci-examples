"! <strong>Beispiel-Start-Konnektor CSV-Daten-Import</strong>
"! <br/>
"! Diese Klasse dient als Start-Konnektor zum Lesen von Datensätzen aus einer
"! CSV-Datei.
"! <br/>
"! Diese Klasse ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 7.3
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
CLASS zcl_mcibuch_startconn_csv_file DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_ishmed_mci_start_connector.

  PROTECTED SECTION.
    "! Initialisiert die Dateizugriffsklasse.
    METHODS initialize_file_access
      RETURNING
        VALUE(r_result) TYPE REF TO zif_mcibuch_file_access.

    "! Ermittelt den Namen der einzulesenden Datei.
    METHODS get_filename
      RETURNING
        VALUE(r_result) TYPE string.

  PRIVATE SECTION.
    "! Die Nachrichtenklasse der eigenen Meldungen.
    CONSTANTS co_message_id TYPE symsgid VALUE 'ZMCIBUCH'.

    "! Instanz zur Kapselung der Dateizugriffe.
    DATA file_access TYPE REF TO zif_mcibuch_file_access.

ENDCLASS.



CLASS ZCL_MCIBUCH_STARTCONN_CSV_FILE IMPLEMENTATION.


  METHOD get_filename.
    r_result = 'import.csv' ##no_text.
  ENDMETHOD.


  METHOD if_ishmed_mci_component~init.
    me->file_access = initialize_file_access( ).
  ENDMETHOD.


  METHOD if_ishmed_mci_start_connector~receive.

    " Dateiname und Dummy-Deskriptor ermitteln
    DATA(filename) = get_filename( ).
    DATA(file_descriptor) = VALUE rsfillst(
      name = filename
    ).

    TRY.
        " Datei öffnen
        me->file_access->open_dataset( filename ).

        " Kopfzeile lesen
        TRY.
            DATA(header_line) = me->file_access->read_dataset( ).

            " Inhalte lesen
            DATA(line_number) = 0.
            DO.
              TRY.
                  ADD 1 TO line_number.
                  DATA(data_line) = me->file_access->read_dataset( ).

                  " Nachricht erstellen und publizieren
                  DATA(message) = NEW zcl_mcibuch_msg_csv_dataset(
                      is_file = file_descriptor
                      i_line  = line_number
                      i_data  = |{ header_line }\r\n{ data_line }|
                  ).
                  RAISE EVENT if_ishmed_mci_start_connector~publish
                    EXPORTING
                      ir_message = message.

                CATCH zcx_mcibuch_eof_reached.
                  IF line_number = 1.
                    " Datei &1 enthält nur eine Kopfzeile
                    ir_logger->warn(
                        i_msgid    = co_message_id
                        i_msgno    = '018'
                        i_msgv1    = filename
                    ).
                  ENDIF.
                  EXIT. " from DO ... ENDDO
              ENDTRY.
            ENDDO.

          CATCH zcx_mcibuch_eof_reached.
            " Datei &1 ist leer
            ir_logger->warn(
                i_msgid    = co_message_id
                i_msgno    = '017'
                i_msgv1    = filename
            ).
        ENDTRY.

        " Datei schließen und löschen
        me->file_access->close_dataset( ).
        me->file_access->delete_dataset( ).

      CATCH zcx_mcibuch_file_not_found.
        " Datei nicht vorhanden - normaler Zustand, kein Fehler
        " Datei &1 nicht vorhanden
        ir_logger->info(
            i_msgid    = co_message_id
            i_msgno    = '016'
            i_msgv1    = filename
        ).
    ENDTRY.

  ENDMETHOD.


  METHOD if_ishmed_mci_start_connector~restart.
    RAISE EVENT if_ishmed_mci_start_connector~publish
      EXPORTING
        ir_message = ir_message.
  ENDMETHOD.


  METHOD if_ishmed_object~finalize.
    FREE me->file_access.
  ENDMETHOD.


  METHOD initialize_file_access.
    r_result = NEW lcl_file_access( ).
  ENDMETHOD.
ENDCLASS.
