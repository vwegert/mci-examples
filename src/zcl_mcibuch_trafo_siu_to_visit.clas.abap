"! <strong>Beispiel-Transformer HL7 SIU-Nachricht in Besuchsanlage</strong>
"! <br/>
"! Diese Klasse ist ein Beispiel zur Entwicklung eines Transformers, der eine
"! HL7-Nachricht mit Hilfe der Klasse CL_ISHMED_MCI_HL7_TO_MSG_MAP auswertet.
"! <br/>
"! Diese Klasse ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 6.3
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
CLASS zcl_mcibuch_trafo_siu_to_visit DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_ishmed_mci_transformer.

  PROTECTED SECTION.

    "! Ermittelt die Bewegungsart, die in den Ausgabedaten eingetragen wird.
    METHODS get_movement_type
      IMPORTING
        i_institution_id TYPE einri
      RETURNING
        VALUE(r_result)  TYPE ri_bwart.

    "! Ermittelt den Bewegungsstatus, der in den Ausgabedaten eingetragen wird.
    METHODS get_movement_status
      IMPORTING
        i_institution_id TYPE einri
      RETURNING
        VALUE(r_result)  TYPE ish_abstex.

  PRIVATE SECTION.

    "! Die von dieser Komponente verarbeitete Nachrichtenstruktur.
    CONSTANTS co_message_structure TYPE n1mci_hl7_message_structure VALUE 'SIU_S12'.

    "! Feldname zur temporären Ablage des Bewegungs-Beginndatums.
    CONSTANTS co_field_temp_start_date TYPE deffdname VALUE '__START_DATE__'.

    "! Feldname zur temporären Ablage der Bewegungs-Beginnzeit.
    CONSTANTS co_field_temp_start_time TYPE deffdname VALUE '__START_TIME__'.

    "! Feldname zur temporären Ablage des Bewegungs-Endedatums.
    CONSTANTS co_field_temp_end_date TYPE deffdname VALUE '__END_DATE__'.

    "! Feldname zur temporären Ablage der Bewegungs-Endezeit.
    CONSTANTS co_field_temp_end_time TYPE deffdname VALUE '__END_TIME__'.

    "! Feldname zur temporären Ablage der Statusangabe.
    CONSTANTS co_field_temp_status TYPE deffdname VALUE '__STATUS__'.

    "! Die Hilfsklasse zur Übertragung der Nachrichteninhalte.
    DATA hl7_map TYPE REF TO lcl_hl7_msg_map.

    "! Verarbeitet die HL7-Nachricht nach Typprüfung.
    METHODS convert_hl7_message
      IMPORTING
        ir_message         TYPE REF TO if_ishmed_mci_hl7_message
        ir_message_context TYPE REF TO if_ishmed_mci_message_context
        ir_logger          TYPE REF TO if_ishmed_mci_logger
      RETURNING
        VALUE(r_result)    TYPE REF TO if_ishmed_mci_message
      RAISING
        zcx_mcibuch_mci_error.

    "! Ermittelt die Steuerungsdaten der Klasse CL_ISHMED_MCI_HL7_TO_MSG_MAP.
    METHODS get_hl7_map_properties
      RETURNING
        VALUE(r_result) TYPE rn1mci_property_t.

    "! Passt die aus der HL7-Nachricht übertragenen Daten an.
    METHODS adjust_mapped_values
      IMPORTING
        ir_message TYPE REF TO zcl_mcibuch_msg_case_visit
        ir_logger  TYPE REF TO if_ishmed_mci_logger ##needed.

    "! Überträgt einen einzelnen Datums- oder Zeitwert.
    METHODS transfer_temporary_value
      IMPORTING
        ir_message    TYPE REF TO zcl_mcibuch_msg_case_visit
        i_field_name  TYPE deffdname
      CHANGING
        c_field_value TYPE simple.

ENDCLASS.



CLASS ZCL_MCIBUCH_TRAFO_SIU_TO_VISIT IMPLEMENTATION.


  METHOD adjust_mapped_values.

    " vorbelegte administrative Bewegungsdaten lesen
    DATA(institution) = ir_message->get_institution( ).
    DATA(visit_data) = ir_message->get_visit_data_adm( ).

    " temporäre Daten aus den zerlegten Zeitstempeln umkopieren und verwerfen
    transfer_temporary_value(
      EXPORTING
        ir_message   = ir_message
        i_field_name = co_field_temp_start_date
      CHANGING
        c_field_value = visit_data-movemnt_date
    ).
    transfer_temporary_value(
      EXPORTING
        ir_message   = ir_message
        i_field_name = co_field_temp_start_time
      CHANGING
        c_field_value = visit_data-movemnt_time
    ).
    transfer_temporary_value(
      EXPORTING
        ir_message   = ir_message
        i_field_name = co_field_temp_end_date
      CHANGING
        c_field_value = visit_data-movemnt_enddate
    ).
    transfer_temporary_value(
      EXPORTING
        ir_message   = ir_message
        i_field_name = co_field_temp_end_time
      CHANGING
        c_field_value = visit_data-movemnt_endtime
    ).

    " Bewegungsart und Status (intern ermittelt) ergänzen
    visit_data-movemnt_type = get_movement_type(
        i_institution_id = institution
    ).
    visit_data-ext_visit_stat = get_movement_status(
        i_institution_id = institution
    ).

    " geänderte Werte zurückschreiben
    ir_message->set_visit_data_adm( visit_data ).

    " Status analysieren und ggf. Storno-Kennzeichen setzen
    DATA status_text TYPE text50.
    transfer_temporary_value(
      EXPORTING
        ir_message   = ir_message
        i_field_name = co_field_temp_status
      CHANGING
        c_field_value = status_text
    ).
    IF to_upper( status_text ) = 'CANCELLED' OR
       to_upper( status_text ) = 'DELETED'.
      ir_message->set_cancelled( abap_true ).
    ENDIF.

  ENDMETHOD.


  METHOD convert_hl7_message.

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

    " neue Zielnachricht erzeugen und Werte übertragen
    TRY.
        DATA(visit_message) = NEW zcl_mcibuch_msg_case_visit( ).
        me->hl7_map->map(
            ir_hl7_message    = ir_message
            ir_message        = visit_message
        ).
        adjust_mapped_values(
            ir_message = visit_message
            ir_logger  = ir_logger
        ).
        " externen Schlüssel setzen
        ir_message_context->set_external_key(
            i_value = visit_message->get_visit_data_adm( )-ext_movement_id
        ).
        r_result = visit_message.
      CATCH cx_ishmed_mci_map INTO DATA(map_exception).
        " Fehler bei der Umsetzung der Nachrichteninhalte
        RAISE EXCEPTION TYPE zcx_mcibuch_mci_error
          EXPORTING
            textid   = zcx_mcibuch_mci_error=>content_mapping_error
            previous = map_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD get_hl7_map_properties.

    DATA(map_properties) = VALUE rn1mci_hl7_to_msg_map_t(
      " MSH-6-1 --> Einrichtung
      ( segment_type            = 'MSH'
        field_number            = 6
        repetition_number       = 1
        component_number        = 1
        sub_component_number    = 1
        dic_type_name_hl7_field = 'EINRI'
        msg_value_name          = VALUE #(
          ( zcl_mcibuch_msg_case_visit=>co_field_institution )
        )
      )

      " PV1-19-1 --> Fallnummer
      ( segment_type            = 'PV1'
        field_number            = 19
        repetition_number       = 1
        component_number        = 1
        sub_component_number    = 1
        dic_type_name_hl7_field = 'FALNR'
        msg_value_name          = VALUE #(
          ( zcl_mcibuch_msg_case_visit=>co_field_case_id )
        )
      )

      " TQ1-7 --> Datum/-zeit der Bewegung
      ( segment_type            = 'TQ1'
        field_number            = 7
        repetition_number       = 1
        component_number        = 1
        sub_component_number    = 1
        dic_type_name_hl7_field = 'CHAR15'
        msg_value_name          = VALUE #(
          ( co_field_temp_start_date )
          ( co_field_temp_start_time )
        )
        dic_type_name_msg       = VALUE #(
          ( 'BWIDT' )
          ( 'BWIZT' )
        )
        split_pattern           = VALUE #(
          ( CONV n1mci_split_pattern( 8 ) )
          ( CONV n1mci_split_pattern( 6 ) )
        )
      )

      " TQ1-8 --> Endedatum/-zeit der Bewegung
      ( segment_type            = 'TQ1'
        field_number            = 8
        repetition_number       = 1
        component_number        = 1
        sub_component_number    = 1
        dic_type_name_hl7_field = 'CHAR15'
        msg_value_name          = VALUE #(
          ( co_field_temp_end_date )
          ( co_field_temp_end_time )
        )
        dic_type_name_msg       = VALUE #(
          ( 'BWEDT' )
          ( 'BWEZT' )
        )
        split_pattern           = VALUE #(
          ( CONV n1mci_split_pattern( 8 ) )
          ( CONV n1mci_split_pattern( 6 ) )
        )
      )

      " AIL-3-4 --> fachliche OE
      ( segment_type            = 'AIL'
        field_number            = 3
        repetition_number       = 1
        component_number        = 4
        sub_component_number    = 1
        dic_type_name_hl7_field = 'NZUWFA'
        msg_value_name          = VALUE #(
          ( zcl_mcibuch_msg_case_visit=>co_field_visit_data_adm )
        )
        dic_type_name_msg       = VALUE #(
          ( 'BAPI2097OVIN' )
        )
        struct_field_name       = 'DEPARTMENT'
      )

      " AIL-3-1 --> pflegerische OE
      ( segment_type            = 'AIL'
        field_number            = 3
        repetition_number       = 1
        component_number        = 1
        sub_component_number    = 1
        dic_type_name_hl7_field = 'NZUWPF'
        msg_value_name          = VALUE #(
          ( zcl_mcibuch_msg_case_visit=>co_field_visit_data_adm )
        )
        dic_type_name_msg       = VALUE #(
          ( 'BAPI2097OVIN' )
        )
        struct_field_name       = 'NURS_TREAT_OU'
      )

      " AIL-3-2 --> Behandlungsraum
      ( segment_type            = 'AIL'
        field_number            = 3
        repetition_number       = 1
        component_number        = 2
        sub_component_number    = 1
        dic_type_name_hl7_field = 'ISH_ZIMMID'
        msg_value_name          = VALUE #(
          ( zcl_mcibuch_msg_case_visit=>co_field_visit_data_adm )
        )
        dic_type_name_msg       = VALUE #(
          ( 'BAPI2097OVIN' )
        )
        struct_field_name       = 'ROOM'
      )

      " SCH-2-1 --> Externe Bewegungsidentifikation
      ( segment_type            = 'SCH'
        field_number            = 2
        repetition_number       = 1
        component_number        = 1
        sub_component_number    = 1
        dic_type_name_hl7_field = 'ISH_EXTNR_BEW'
        msg_value_name          = VALUE #(
          ( zcl_mcibuch_msg_case_visit=>co_field_visit_data_adm )
        )
        dic_type_name_msg       = VALUE #(
          ( 'BAPI2097OVIN' )
        )
        struct_field_name       = 'EXT_MOVEMENT_ID'
      )

      " SCH-25-1 --> Status
      ( segment_type            = 'SCH'
        field_number            = 25
        repetition_number       = 1
        component_number        = 1
        sub_component_number    = 1
        dic_type_name_hl7_field = 'TEXT50'
        msg_value_name          = VALUE #(
          ( co_field_temp_status )
        )
      )

      " PV1-7-1 --> Personalnummer behandelnder Arzt
      ( segment_type            = 'PV1'
        field_number            = 7
        repetition_number       = 1
        component_number        = 1
        sub_component_number    = 1
        dic_type_name_hl7_field = 'BARNR'
        msg_value_name          = VALUE #(
          ( zcl_mcibuch_msg_case_visit=>co_field_visit_data_adm )
        )
        dic_type_name_msg       = VALUE #(
          ( 'BAPI2097OVIN' )
        )
        struct_field_name       = 'ATT_PHYS'
      )
    ).

    DATA map_properties_as_xml TYPE string.
    CALL TRANSFORMATION id
      SOURCE data = map_properties
      RESULT XML map_properties_as_xml.

    r_result = VALUE #(
      ( name  = cl_ishmed_tpi_doc_transformer=>co_map_properties
        value = map_properties_as_xml )
    ).

  ENDMETHOD.


  METHOD get_movement_status ##needed.
    " ... hier Logik zur Ermittlung des Bewegungsstatus einsetzen ...
  ENDMETHOD.


  METHOD get_movement_type ##needed.
    " ... hier Logik zur Ermittlung der Bewegungsart einsetzen ...
  ENDMETHOD.


  METHOD if_ishmed_mci_component~init.

    " HL7-Mapper initialisieren
    me->hl7_map = NEW lcl_hl7_msg_map( ).
    me->hl7_map->if_ishmed_mci_component~init(
        i_type             = if_ishmed_mci_comp_constant=>co_transformer
        ir_logger          = ir_logger
        it_properties      = get_hl7_map_properties( )
    ).

  ENDMETHOD.


  METHOD if_ishmed_mci_transformer~transform.

    " MCI-Nachrichtentyp prüfen - wir erwarten eine HL7-Nachricht
    "   Hinweis: Ab Basis-Release 7.50 geht das einfacher:
    "   IF cr_message IS INSTANCE OF if_ishmed_mci_hl7_message.
    DATA(hl7_message_descriptor) = CAST cl_abap_intfdescr(
        cl_abap_typedescr=>describe_by_name( 'IF_ISHMED_MCI_HL7_MESSAGE' )
    ).
    IF hl7_message_descriptor->applies_to( cr_message ).
      cr_message = convert_hl7_message(
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


  METHOD if_ishmed_object~finalize.

    " HL7-Mapper abbauen
    me->hl7_map->if_ishmed_object~finalize( ).

  ENDMETHOD.


  METHOD transfer_temporary_value.
    TRY.
        ir_message->get_data(
          EXPORTING
            i_name              = i_field_name
          IMPORTING
            e_value             = c_field_value
        ).
        " bei Datums- und Zeitangaben fehlende endstehende Nullen wieder ergänzen
        DESCRIBE FIELD c_field_value TYPE DATA(field_type).
        IF field_type CA 'DT'.
          TRANSLATE c_field_value USING ' 0'.
        ENDIF.
        ir_message->clear_data( i_field_name ).
      CATCH cx_ishmed_not_found.
        CLEAR c_field_value.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
