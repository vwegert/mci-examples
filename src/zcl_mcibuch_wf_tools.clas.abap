"! <strong>Hilfsklasse für die Workflow-Integration</strong>
"! <br/>
"! Diese Klasse stellt eine Methode zum Aufruf einer Nachricht im MCI-Monitor
"! zur Verfügung.
"! <br/>
"! Diese Klasse  ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 10.6.3
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
CLASS zcl_mcibuch_wf_tools DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES bi_object.
    INTERFACES bi_persistent.
    INTERFACES if_workflow.

    "! Aufruf des MCI-Monitors zu einer angegebenen Nachricht.
    CLASS-METHODS show_mci_monitor
      IMPORTING
        message TYPE REF TO cl_ishmed_wf_mci_message
      RAISING
        cx_bo_error.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_MCIBUCH_WF_TOOLS IMPLEMENTATION.


  METHOD bi_object~default_attribute_value ##needed.
  ENDMETHOD.


  METHOD bi_object~execute_default_method ##needed.
  ENDMETHOD.


  METHOD bi_object~release ##needed.
  ENDMETHOD.


  METHOD bi_persistent~find_by_lpor.
    " Wir nutzen diese Klasse nur als Werkzeug und laden keine Instanzdaten zum
    " Beispiel aus der Datenbank nach. Daher kann unabhängig von der LPOR immer
    " eine frische Instanz zurückgegeben werden.
    result = NEW zcl_mcibuch_wf_tools( ).
  ENDMETHOD.


  METHOD bi_persistent~lpor.
    " Die LPOR ist der eindeutige Identifier im Workflow. Nach dieser ID gefragt
    " muss unsere Klasse etwas antworten. Da wir keine Instanzdaten verarbeiten,
    " darf dies auch eine statische Instanz-ID sein.
    result = VALUE #(
      instid = 'DUMMY' ##no_text
      catid =  swfco_objtype_cl
      typeid = 'ZCL_MCIBUCH_WF_TOOLS' ##no_text
    ).

  ENDMETHOD.


  METHOD bi_persistent~refresh ##needed.
  ENDMETHOD.


  METHOD show_mci_monitor.

    " Damit die Nachrichtenselektion valide Ergebnisse liefert,
    " kann an dieser Stelle nur auf folgende Eigenschaften zugegriffen werden:
    " - Prozessname
    " - Nachrichtenstatus
    " - Start-/Endzeitpunkt der Nachrichtenverarbeitung

    " Nachrichtendaten nachladen
    TRY.
        DATA(message_data) =
            cl_ishmed_mci_message_data=>load( message->key-message_id ).
      CATCH cx_ishmed_not_found
            cx_ishmed_data
            cx_ishmed_object INTO DATA(exception).

        " Wenn eine Exception geworfen wird, WF Aufgabe abbrechen und Exception
        " im Protokoll ausgeben
        " Besser wäre hier eine eigene Klasse welche von CX_BO_ERROR erbt
        " sprechende Nachrichten ausgibt
        RAISE EXCEPTION TYPE cx_bo_error
          EXPORTING
            previous = exception.
    ENDTRY.

    " Start-Zeitpunkt aus den Nachrichtendaten ermitteln
    DATA(start_timestamp) = message_data->get_start_timestamp( ).
    CONVERT TIME STAMP start_timestamp TIME ZONE sy-zonlo
      INTO DATE DATA(start_date)
           TIME DATA(start_time).

    " End-Zeitpunkt aus den Nachrichtendaten ermittlen
    DATA(end_timestamp) = message_data->get_end_timestamp( ).
    end_timestamp = end_timestamp + 1.
    CONVERT TIME STAMP end_timestamp TIME ZONE sy-zonlo
      INTO DATE DATA(end_date)
           TIME DATA(end_time).

    " Selektionsparameter aufbauen
    DATA(execute) = abap_true.
    DATA(parameter) = VALUE rsparams_tt(

      " Start-Datum
      ( selname = 'P_STARTD'
        kind    = 'P'
        sign    = 'I'
        option  = 'EQ'
        low     = start_date
      )

      " Start-Zeit
      ( selname = 'P_STARTT'
        kind    = 'P'
        sign    = 'I'
        option  = 'EQ'
        low     = start_time
      )

      " Ende-Datum
      ( selname = 'P_ENDD'
        kind    = 'P'
        sign    = 'I'
        option  = 'EQ'
        low     = end_date
      )

      " Ende-Zeit
      ( selname = 'P_ENDT'
        kind    = 'P'
        sign    = 'I'
        option  = 'EQ'
        low     = end_time
      )

      " Prozessname
      ( selname = 'SO_PROC'
        kind    = 'S'
        sign    = 'I'
        option  = 'EQ'
        low     = message->process_name
       )

       " Status
      ( selname = 'SO_STAT'
        kind    = 'S'
        sign    = 'I'
        option  = 'EQ'
        low     = message->status
       )
    ).

    " Parameter in den Speicher stellen
    EXPORT parameter FROM parameter TO MEMORY ID 'N1MCI_MONITOR'.
    EXPORT execute   FROM execute    TO MEMORY ID 'N1MCI_MONITOR_EXEC'.

    " MCI-Monitor rufen
    CALL TRANSACTION 'N1MCI_MONITOR' WITH AUTHORITY-CHECK.

  ENDMETHOD.
ENDCLASS.
