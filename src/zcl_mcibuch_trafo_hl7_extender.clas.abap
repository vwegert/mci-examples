"! <strong>Beispiel-Transformer zur Erweiterung einer HL7-Nachricht</strong>
"! <br/>
"! Diese Klasse stellt ein Beispiel eines einfachen Transformers dar, der eine
"! HL7-Nachricht um weitere Daten anreichert.
"! <br/>
"! Diese Klasse ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 6.1
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
CLASS zcl_mcibuch_trafo_hl7_extender DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_ishmed_mci_transformer.

  PROTECTED SECTION.

  PRIVATE SECTION.

    " Die Nachrichtenklasse der eigenen Meldungen.
    CONSTANTS co_message_id TYPE symsgid VALUE 'ZMCIBUCH'.

    "! Die von dieser Komponente verarbeitete Nachrichtenstruktur.
    CONSTANTS co_message_structure TYPE n1mci_hl7_message_structure VALUE 'MDM_T02'.

    "! Das Kodier-System zur Angabe von LOINC-Schlüsseln.
    CONSTANTS co_coding_system_loinc TYPE string VALUE 'LN'.

    "! Das Kodier-System zur Angabe von Einheiten nach ISO 31.
    CONSTANTS co_coding_system_units TYPE string VALUE 'ISO31'.

    "! Das Kodier-System zur Angabe von lokalen Schlüsseln.
    CONSTANTS co_coding_system_local TYPE string VALUE 'L'.

    " Erweitert eine BAR_P12-Nachricht um OBX-Segmente für Größe und Gewicht.
    METHODS process_hl7_message
      IMPORTING
        ir_message         TYPE REF TO if_ishmed_mci_hl7_message
        ir_message_context TYPE REF TO if_ishmed_mci_message_context
        ir_logger          TYPE REF TO if_ishmed_mci_logger
      RAISING
        zcx_mcibuch_mci_error.

    " Ermittelt die Falldaten anhand der Schlüssel aus dem Kontext.
    METHODS read_case_data
      IMPORTING
        ir_message_context TYPE REF TO if_ishmed_mci_message_context
      RETURNING
        VALUE(r_case_data) TYPE nfal
      RAISING
        zcx_mcibuch_mci_error.

    " Prüft die eingehende HL7-Nachricht und ermittelt Daten zur Verarbeitung.
    "! @parameter e_insert_position | die Position, an der die zusätzlichen
    "!                                OBX-Segmente eingefügt werden sollen
    "! @parameter e_max_existing_set_id | die höchste bisher vergebene Set-ID
    METHODS examine_existing_message
      IMPORTING
        ir_message            TYPE REF TO if_ishmed_mci_hl7_message
        ir_logger             TYPE REF TO if_ishmed_mci_logger
      EXPORTING
        e_insert_position     TYPE i
        e_max_existing_set_id TYPE i
      RAISING
        zcx_mcibuch_mci_error.

    "! Ermittelt die Set-ID aus einem OBX-Segment (inkl. Fehlerbehandlung).
    METHODS read_set_id_from_obx_segment
      IMPORTING
        ir_logger       TYPE REF TO if_ishmed_mci_logger
        ir_segment      TYPE REF TO if_ishmed_mci_hl7_segment
        i_position      TYPE i
      RETURNING
        VALUE(r_set_id) TYPE i.

    "! Erzeugt ein OBX-Segment mit LOINC-Kodierung, numerischem Wert und Einheit.
    METHODS create_numeric_obx_segment
      IMPORTING
        i_set_id        TYPE i
        i_loinc_code    TYPE simple
        i_code_text     TYPE simple
        i_value         TYPE simple
        i_unit          TYPE simple
      RETURNING
        VALUE(r_result) TYPE REF TO if_ishmed_mci_hl7_segment.

ENDCLASS.



CLASS ZCL_MCIBUCH_TRAFO_HL7_EXTENDER IMPLEMENTATION.


  METHOD create_numeric_obx_segment.

    DATA(segment) = NEW cl_ishmed_mci_hl7_seg_obx_26(
        i_set_id     = i_set_id
        i_value_type = 'NM'
    ).

    " OBX-3 Observation Identifier
    segment->set_observation_identifier(
        i_identifier                   = i_loinc_code
        i_text                         = i_code_text
        i_name_of_coding_system        = co_coding_system_loinc
    ).

    " OBX-5 Observation Value
    " unabhängig von der Prozess-Sprache Wert immer 'Englisch' formatieren
    CALL FUNCTION 'SCP_MIXED_LANGUAGES_1_INIT'.
    CALL FUNCTION 'SCP_MIXED_LANGUAGES_1_SWITCH'
      EXPORTING
        need_lang = 'E'.
    DATA string_value TYPE c LENGTH 100.
    WRITE i_value TO string_value UNIT i_unit.
    CALL FUNCTION 'SCP_MIXED_LANGUAGES_1_FINISH'.
    segment->set_observation_value( condense( string_value ) ).

    " OBX-6 Units

    " Text der Einheit lesen
    DATA unit_text TYPE msehl.
    CALL FUNCTION 'UNIT_OF_MEASUREMENT_TEXT_GET'
      EXPORTING
        i_msehi = i_unit
      IMPORTING
        e_msehl = unit_text
      EXCEPTIONS
        OTHERS  = 1.
    IF sy-subrc <> 0.
      unit_text = |???{ i_unit }???|.
    ENDIF.

    " ISO-Code der Einheit lesen
    DATA iso_code TYPE  isocd_unit.
    CALL FUNCTION 'UNIT_OF_MEASURE_SAP_TO_ISO'
      EXPORTING
        sap_code = i_unit
      IMPORTING
        iso_code = iso_code
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      CLEAR iso_code.
    ENDIF.

    " Werte in das Einheiten-Feld übertragen
    segment->set_units(
        i_identifier                   = iso_code
        i_text                         = unit_text
        i_name_of_coding_system        = co_coding_system_units
        i_alternate_identifier         = i_unit
        i_alternate_text               = unit_text
        i_name_of_alt_coding_system    = co_coding_system_local
    ).

    r_result = segment.

  ENDMETHOD.


  METHOD examine_existing_message.

    " Startwerte festlegen - siehe Fehlerprüfung unten
    e_insert_position = -1.
    e_max_existing_set_id = 0.
    DATA(txa_encountered) = abap_false.

    " alle Segmente mit Iterator durchlaufen
    DATA(iterator) = ir_message->get_segment_collection( )->get_iterator( ).
    DATA(position) = 0.
    WHILE iterator->has_next( ).
      TRY.
          DATA(current_segment) = iterator->get_next( ).
          position = position + 1.
        CATCH cx_ishmed_illegal_position.
          " Sollte nicht passieren, da wir mit has_next( ) die Verfügbarkeit
          " weiterer Segmente geprüft haben --> Verarbeitung beenden.
          RETURN.
      ENDTRY.

      " Segment-Typ prüfen
      CASE current_segment->get_type( ).
        WHEN 'TXA'.
          " Falls nach dem TXA-Segment kein OBX-Segment mehr folgen sollte, halten
          " wir die Einfügeposition nach dem TXA-Segment schon einmal fest.
          e_insert_position = position + 1.
          txa_encountered = abap_true.

        WHEN 'OBX'.
          " Einfügeposition ist auf jeden Fall hinter diesem Segment
          " (falls weitere OBX-Segmente folgen, dann hinter diesen...)
          e_insert_position = position + 1.

          " Set-ID aus OBX-1 auslesen und überprüfen
          DATA(current_set_id) = read_set_id_from_obx_segment(
              ir_logger  = ir_logger
              i_position = position
              ir_segment = current_segment
          ).
          " falls Set-ID größer ist, als Ergebnis merken
          IF current_set_id > e_max_existing_set_id.
            e_max_existing_set_id = current_set_id.
          ENDIF.

        WHEN 'NTE'.
          " Falls das NTE-Segment auf ein OBX-Segment folgt, dürfen wir das neue
          " Segment nicht zwischen OBX und NTE einfügen. NTE-Segmente verschieben
          " die Einfügeposition ebenfalls nach hinten - sofern sie nach dem TXA-
          " Segment und den dazugehörigen OBX-Segmenten stehen und nicht
          " zu dem vor dem TXA-Segment stehenden OBR-Segment gehören.
          IF txa_encountered = abap_true.
            e_insert_position = position + 1.
          ENDIF.

      ENDCASE.
    ENDWHILE.

    IF txa_encountered = abap_false OR e_insert_position < 0.
      " Die Nachricht enthält das erforderliche TXA-Segment nicht
      RAISE EXCEPTION TYPE zcx_mcibuch_mci_error
        EXPORTING
          textid = zcx_mcibuch_mci_error=>txa_segment_missing.
    ENDIF.

  ENDMETHOD.


  METHOD if_ishmed_mci_component~init ##needed.
  ENDMETHOD.


  METHOD if_ishmed_mci_transformer~transform.

    " MCI-Nachrichtentyp prüfen - wir erwarten eine HL7-Nachricht
    "   Hinweis: Ab Basis-Release 7.50 geht das einfacher:
    "   IF cr_message IS INSTANCE OF if_ishmed_mci_hl7_message.
    DATA(hl7_message_descriptor) = CAST cl_abap_intfdescr(
        cl_abap_typedescr=>describe_by_name( 'IF_ISHMED_MCI_HL7_MESSAGE' )
    ).
    IF hl7_message_descriptor->applies_to( cr_message ).
      process_hl7_message(
          ir_message         = CAST if_ishmed_mci_hl7_message( cr_message )
          ir_message_context = ir_message_context
          ir_logger          = ir_logger
      ).
    ELSE.
      " Die übergebene Nachricht ist keine HL7-Nachricht
      RAISE EXCEPTION TYPE zcx_mcibuch_mci_error
        EXPORTING
          textid = zcx_mcibuch_mci_error=>message_type_not_hl7.
    ENDIF.

  ENDMETHOD.


  METHOD if_ishmed_object~finalize ##needed.
    " Hier eventuelle Bereinigungs-Operationen implementieren.
  ENDMETHOD.


  METHOD process_hl7_message.

    " Nachrichtenstruktur prüfen
    DATA(message_type) = ir_message->get_message_type( ).
    IF message_type-message_structure <> co_message_structure.
      " Falsche HL7-Nachrichtenstruktur &ACTUAL_MSG_STRUCTURE&
      " (erwartet: &EXPECTED_MSG_STRUCTURE&)
      RAISE EXCEPTION TYPE zcx_mcibuch_mci_error
        EXPORTING
          textid                 = zcx_mcibuch_mci_error=>unsupported_msg_struct
          expected_msg_structure = co_message_structure
          actual_msg_structure   = message_type-message_structure.
    ENDIF.

    " Falldaten lesen und nur fortfahren, wenn Größe/Gewicht vorhanden ist
    DATA(case_data) = read_case_data( ir_message_context ).
    IF case_data-patgew IS NOT INITIAL OR case_data-patgro IS NOT INITIAL.

      " Einfügeposition und höchste vorhandene Set-ID ermitteln
      examine_existing_message(
        EXPORTING
          ir_message              = ir_message
          ir_logger               = ir_logger
        IMPORTING
          e_insert_position       = DATA(insert_position)
          e_max_existing_set_id   = DATA(max_existing_set_id)
      ).
      DATA(next_set_id) = max_existing_set_id + 1.

      " OBX-Segment für Größe erzeugen und einfügen
      IF case_data-patgro IS NOT INITIAL.
        ir_message->get_segment_collection( )->insert(
            i_index    = insert_position
            ir_segment = create_numeric_obx_segment(
                i_set_id     = next_set_id
                i_loinc_code = '8302-2'
                i_code_text  = 'Größe'(001)
                i_value      = case_data-patgro
                i_unit       = case_data-grein
            )
        ).
        insert_position = insert_position + 1.
        next_set_id = next_set_id + 1.
      ENDIF.

      " OBX-Segment für Gewicht erzeugen und einfügen
      IF case_data-patgew IS NOT INITIAL.
        ir_message->get_segment_collection( )->insert(
            i_index    = insert_position
            ir_segment = create_numeric_obx_segment(
                i_set_id     = next_set_id
                i_loinc_code = '29463-7'
                i_code_text  = 'Gewicht'(002)
                i_value      = case_data-patgew
                i_unit       = case_data-gwein
            )
        ).
        insert_position = insert_position + 1.
        next_set_id = next_set_id + 1.
      ENDIF.

    ELSE.
      " Zu Fall &1 in Einrichtung &2 sind weder Größe noch Gewicht dokumentiert
      ir_logger->info(
          i_msgid    = co_message_id
          i_msgno    = '009'
          i_msgv1    = case_data-falnr
          i_msgv2    = case_data-einri
      ).
    ENDIF.

  ENDMETHOD.


  METHOD read_case_data.

    " Einrichtung und Fall aus dem Nachrichtenkontext ermitteln
    DATA(institution_id) = ir_message_context->get_einri( ).
    DATA(case_id) = ir_message_context->get_falnr( ).
    IF institution_id IS INITIAL OR case_id IS INITIAL.
      " Nachrichtenkontext enthält keine Einrichtung und/oder keine Fallnummer
      RAISE EXCEPTION TYPE zcx_mcibuch_mci_error
        EXPORTING
          textid = zcx_mcibuch_mci_error=>institution_or_case_missing.
    ENDIF.

    " Falldaten ermitteln
    CALL FUNCTION 'ISH_READ_NFAL'
      EXPORTING
        ss_einri = institution_id
        ss_falnr = case_id
      IMPORTING
        ss_nfal  = r_case_data
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      " Fehler beim Zugriff auf Fall &CASE_ID& in Einrichtung &INSTITUTION_ID&
      RAISE EXCEPTION TYPE zcx_mcibuch_mci_error
        EXPORTING
          textid         = zcx_mcibuch_mci_error=>case_read_error
          institution_id = institution_id
          case_id        = case_id.
    ENDIF.

  ENDMETHOD.


  METHOD read_set_id_from_obx_segment.

    " sicherheitshalber nicht davon ausgehen, dass die Set-ID gültig ist - deshalb
    " erst als String auslesen und dann konvertieren

    DATA current_set_id_string TYPE string.
    TRY.
        ir_segment->get_field_value(
          EXPORTING
            i_number = 1
          IMPORTING
            e_value  = current_set_id_string
        ).
      CATCH cx_ishmed_illegal_position.
        " darf eigentlich nicht passieren, werten wir als "nicht angegeben"
        CLEAR current_set_id_string.
        " HL7-Nachricht enthält an Position &1 OBX-Segment mit ungültiger Set-ID
        ir_logger->warn(
            i_msgid    = co_message_id
            i_msgno    = '010'
            i_msgv1    = i_position
        ).
    ENDTRY.

    IF current_set_id_string IS NOT INITIAL.
      TRY.
          r_set_id  = CONV i( current_set_id_string ).
        CATCH cx_sy_conversion_error INTO DATA(conversion_error).
          " HL7-Nachricht enthält an Position &1 OBX-Segment mit ungültiger Set-ID
          ir_logger->warn(
              i_msgid    = co_message_id
              i_msgno    = '010'
              i_msgv1    = i_position
          ).
          cl_ishmed_mci_utl=>log_exception(
              ir_logger    = ir_logger
              ix_exception = conversion_error
          ).
          CLEAR r_set_id.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
