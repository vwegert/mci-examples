"! <strong>Beispiel für lesende Verwendung des HL7-Objektmodells</strong>
"! <br/>
"! Bei dieser Klasse handelt es sich nicht um einen voll ausgebildeten Transformer
"! zur Verarbeitung produktiver HL7-Nachrichten. Hier soll lediglich die Verwendung
"! des HL7-Objektmodells zum Lesezugriff auf bereits bestehende Nachrichten
"! demonstriert werden.
"! <br/>
"! Diese Klasse ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 5.2.2
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
CLASS zcl_mcibuch_hl7_read_demo DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_ishmed_mci_transformer.

  PROTECTED SECTION.

  PRIVATE SECTION.

    " Die Nachrichtenklasse der eigenen Meldungen.
    CONSTANTS co_message_id TYPE symsgid VALUE 'ZMCIBUCH'.

    "! Die minimale HL7-Version, die diese Komponente verarbeitet.
    CONSTANTS co_min_hl7_version TYPE n1mci_hl7_version_id VALUE '2.5'.

    "! Die von dieser Komponente verarbeitete Nachrichtenstruktur.
    CONSTANTS co_message_structure TYPE n1mci_hl7_message_structure VALUE 'BAR_P12'.

    "! Der Identifier Type Code (Tabelle 0203) der LANR.
    CONSTANTS co_type_code_lanr TYPE string VALUE 'LANR'.

    "! Prüft den Nachrichtentyp der HL7-Nachricht und übergibt die Nachricht zur
    "! weiteren Verarbeitung.
    METHODS process_hl7_message
      IMPORTING
        ir_message TYPE REF TO if_ishmed_mci_hl7_message
        ir_logger  TYPE REF TO if_ishmed_mci_logger
      RAISING
        cx_ishmed_mci.

    "! Verarbeitet den Nachrichteninhalt und sucht die PR1-/ROL-Paare.
    METHODS process_segments
      IMPORTING
        ir_segments TYPE REF TO if_ishmed_mci_hl7_segment_col
        ir_logger   TYPE REF TO if_ishmed_mci_logger
      RAISING
        cx_ishmed_mci.

    "! Verarbeitet ein ROL-Segment mit zugehörigem PR1-Segment.
    METHODS process_rol_segment
      IMPORTING
        i_rol_position TYPE i
        ir_rol_segment TYPE REF TO if_ishmed_mci_hl7_segment
        i_pr1_position TYPE i
        ir_pr1_segment TYPE REF TO if_ishmed_mci_hl7_segment
        ir_logger      TYPE REF TO if_ishmed_mci_logger
      RAISING
        cx_ishmed_mci.

    " Legt die ermittelte Zuordnung zur weiteren Verarbeitung ab.
    METHODS store_proc_person_assignment
      IMPORTING
        i_procedure_identifier TYPE string ##needed
        i_role_action_code     TYPE string
        i_role_function_id     TYPE string
        i_role_function_text   TYPE string
        i_person_identifier    TYPE string
        i_person_family_name   TYPE string
        i_person_given_name    TYPE string.

    " Fehler beim Zugriff auf ein Einzelfeld protokollieren
    METHODS log_field_access_error
      IMPORTING
        i_segment_type     TYPE n1mci_hl7_segment_type
        i_segment_position TYPE i
        ir_logger          TYPE REF TO if_ishmed_mci_logger
        ir_exception       TYPE REF TO cx_ishmed_illegal_position.

ENDCLASS.



CLASS ZCL_MCIBUCH_HL7_READ_DEMO IMPLEMENTATION.


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
          ir_message = CAST if_ishmed_mci_hl7_message( cr_message )
          ir_logger  = ir_logger
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


  METHOD log_field_access_error.
    " Strukturfehler: Fehler beim Feldzugriff in &1-Segment an Position &2
    ir_logger->error(
        i_msgid    = co_message_id
        i_msgno    = '007'
        i_msgv1    = i_segment_type
        i_msgv2    = i_segment_position
    ).
    cl_ishmed_mci_utl=>log_exception(
        ir_logger    = ir_logger
        ix_exception = ir_exception
    ).
  ENDMETHOD.


  METHOD process_hl7_message.

    " HL7-Version und -Nachrichtenstruktur prüfen: Wir erwarten eine
    " BAR_P12-Nachricht in HL7-Version 2.5 oder höher.
    DATA(hl7_version) = ir_message->get_version_id( ).
    IF hl7_version < co_min_hl7_version.
      " Falsche HL7-Version &ACTUAL_VERSION&
      " (erwartet: Version &EXPECTED_MIN_VERSION& oder höher)
      RAISE EXCEPTION TYPE zcx_mcibuch_mci_error
        EXPORTING
          textid               = zcx_mcibuch_mci_error=>unsupported_hl7_version
          expected_min_version = co_min_hl7_version
          actual_version       = hl7_version.
    ENDIF.
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

    " Segmente der Nachricht verarbeiten
    process_segments(
        ir_segments = ir_message->get_segment_collection( )
        ir_logger   = ir_logger
    ).

  ENDMETHOD.


  METHOD process_rol_segment.

    " Daten zur Zuordnung sammeln:
    "  - ID der Prozedur
    "  - Aktionscode des ROL-Segments
    "  - Funktion mit Schlüssel und Text
    "  - Name, Vorname und LANR des Arztes

    " Voraussetzungen:
    "  - nur eine Person je ROL-Segment; mehrere XCN-Werte beziehen sich auf
    "    die gleiche Person
    "  - Identifier Type Code: non-Standard-Wert LANR

    TRY.
        " Procedure Identifier (PR1-19): Typ EI, Komponente 1 (Entity Identifier)
        DATA procedure_identifier TYPE string.
        ir_pr1_segment->get_field_value(
          EXPORTING
            i_number                   = 19
          IMPORTING
            e_value                    = procedure_identifier
        ).
      CATCH cx_ishmed_illegal_position INTO DATA(field_position_error).
        " Fehler melden und Verarbeitung abbrechen
        log_field_access_error(
            i_segment_type     = 'PR1'
            i_segment_position = i_pr1_position
            ir_logger          = ir_logger
            ir_exception       = field_position_error
        ).
        RETURN.
    ENDTRY.

    TRY.
        " Role Action Code (ROL-2): Typ ID, direkt verwendbar
        DATA role_action_code TYPE string.
        ir_rol_segment->get_field_value(
          EXPORTING
            i_number                   = 2
          IMPORTING
            e_value                    = role_action_code
        ).

        " Role Function (ROL-3): Typ CE, Komponente 1 ID, Komponente 2 Text
        DATA role_function_id TYPE string.
        DATA role_function_text TYPE string.
        DATA(role_function_field) = ir_rol_segment->get_field( 3 ).
        role_function_field->get_value(
          EXPORTING
            i_component_number     = 1
          IMPORTING
            e_value                = role_function_id
        ).
        role_function_field->get_value(
          EXPORTING
            i_component_number     = 2
          IMPORTING
            e_value                = role_function_text
        ).

        " Role Person (ROL-4): Typ XCN, wiederholbar
        " --> alle Wiederholungen nach LANR durchsuchen
        DATA(role_person_field) = ir_rol_segment->get_field( 4 ).
        DATA(role_person_repetition) = 1.
        WHILE role_person_repetition <= role_person_field->get_repetition_count( ).

          " filtern auf Identifier Type Code (XCN-13) = LANR
          DATA person_id_type_code TYPE string.
          role_person_field->get_value(
            EXPORTING
              i_repetition_number    = role_person_repetition
              i_component_number     = 13
            IMPORTING
              e_value                = person_id_type_code
          ).
          IF person_id_type_code = co_type_code_lanr.

            " ID Number = LANR (XCN-1)
            DATA person_identifier TYPE string.
            role_person_field->get_value(
              EXPORTING
                i_repetition_number    = role_person_repetition
                i_component_number     = 1
              IMPORTING
                e_value                = person_identifier
            ).

            " Family Name (XCN-2)
            DATA person_family_name TYPE string.
            role_person_field->get_value(
              EXPORTING
                i_repetition_number    = role_person_repetition
                i_component_number     = 2
              IMPORTING
                e_value                = person_family_name
            ).

            " Given Name (XCN-3)
            DATA person_given_name TYPE string.
            role_person_field->get_value(
              EXPORTING
                i_repetition_number    = role_person_repetition
                i_component_number     = 3
              IMPORTING
                e_value                = person_given_name
            ).

          ENDIF. " person_id_type_code = co_type_code_lanr.

          role_person_repetition = role_person_repetition + 1.
        ENDWHILE. " ..._repetition <= ..._field->get_repetition_count( )
      CATCH cx_ishmed_illegal_position INTO field_position_error.
        " Fehler melden und Verarbeitung abbrechen
        log_field_access_error(
            i_segment_type     = 'PR1'
            i_segment_position = i_pr1_position
            ir_logger          = ir_logger
            ir_exception       = field_position_error
        ).
        RETURN.
    ENDTRY.

    " Daten nur verarbeiten, wenn LANR vorhanden, sonst Fehler melden
    IF person_identifier IS INITIAL.
      " Inhaltsfehler: ROL-Segment (Position &1) enthält keine LANR
      ir_logger->error(
          i_msgid    = co_message_id
          i_msgno    = '006'
          i_msgv1    = i_rol_position
      ).
    ELSE.
      store_proc_person_assignment(
          i_procedure_identifier = procedure_identifier
          i_role_action_code     = role_action_code
          i_role_function_id     = role_function_id
          i_role_function_text   = role_function_text
          i_person_identifier    = person_identifier
          i_person_family_name   = person_family_name
          i_person_given_name    = person_given_name
      ).
    ENDIF.

  ENDMETHOD.


  METHOD process_segments.

    " alle Segmente mit Iterator durchlaufen
    DATA(iterator) = ir_segments->get_iterator( ).
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
        WHEN 'PR1'.
          " Prozeduren-Segment als Referenz sichern
          DATA(preceding_pr1_segment) = current_segment.
          DATA(pr1_position) = position.
        WHEN 'ROL'.
          " ROL-Segment zur weiteren Analyse zusammen mit dem vorherigen
          " PR1-Segment übergeben
          IF preceding_pr1_segment IS NOT INITIAL.
            " Segment-Paar weiter untersuchen
            process_rol_segment(
                i_rol_position = position
                ir_rol_segment = current_segment
                i_pr1_position = pr1_position
                ir_pr1_segment = preceding_pr1_segment
                ir_logger      = ir_logger
            ).
          ELSE.
            " Fehler melden, OHNE Verarbeitung abzubrechen
            " Strukturfehler: ROL-Segment (Position &1) ohne
            " vorhergehendes PR1-Segment
            ir_logger->error(
                i_msgid    = co_message_id
                i_msgno    = '005'
                i_msgv1    = position
            ).
          ENDIF.
        WHEN OTHERS.
          " ROL-Segmente sollten nur direkt nach PR1-Segmenten
          " berücksichtigt werden
          CLEAR preceding_pr1_segment.
          CLEAR pr1_position.
      ENDCASE.
    ENDWHILE.

  ENDMETHOD.


  METHOD store_proc_person_assignment ##needed.
    " Hier würden die extrahierten Daten weiter verarbeitet...
  ENDMETHOD.
ENDCLASS.
