"! <strong>Beispiel zur Erzeugung einer neuen HL7-Nachricht</strong>
"! <br/>
"! Verwenden Sie das Programm ZMCIBUCH_HL7_CREATE_HL7_MSG, um dieses Beispiel
"! auszuführen.
"! <br/>
"! Diese Klasse ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 5.2.4
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
CLASS zcl_mcibuch_hl7_create_demo DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    "! Erzeugt die Nachricht und zeigt sie an.
    METHODS execute
      IMPORTING
        i_institution_id TYPE einri
        i_case_id        TYPE falnr.

  PROTECTED SECTION.

  PRIVATE SECTION.

    "! Erzeugt die HL7-Nachricht der Struktur CSR_C01.
    METHODS create_message
      IMPORTING
        i_institution_id TYPE einri
        i_case_id        TYPE falnr
      RETURNING
        VALUE(r_result)  TYPE REF TO if_ishmed_mci_hl7_message
      RAISING
        cx_ishmed_mci
        cx_ishmed_illegal_position.

    "! Erzeugt und befüllt das MSH-Segment.
    METHODS create_msh_segment
      RETURNING
        VALUE(r_result) TYPE REF TO if_ishmed_mci_hl7_segment
      RAISING
        cx_ishmed_mci.

    "! Erzeugt und befüllt das PV1-Segment.
    METHODS create_pv1_segment
      IMPORTING
        i_case_data     TYPE nfal
      RETURNING
        VALUE(r_result) TYPE REF TO if_ishmed_mci_hl7_segment
      RAISING
        cx_ishmed_mci
        cx_ishmed_illegal_position.

    "! Erzeugt und befüllt das CSR-Segment.
    METHODS create_csr_segment
      IMPORTING
        i_case_data     TYPE nfal
      RETURNING
        VALUE(r_result) TYPE REF TO if_ishmed_mci_hl7_segment
      RAISING
        cx_ishmed_mci
        cx_ishmed_illegal_position.

    "! Liest die Falldaten.
    METHODS read_case_data
      IMPORTING
        i_institution_id   TYPE einri
        i_case_id          TYPE falnr
      RETURNING
        VALUE(r_case_data) TYPE nfal
      RAISING
        zcx_mcibuch_mci_error.

    "! "Ermittelt" Studiendaten (generiert Beispielwerte).
    METHODS get_study_id
      RETURNING
        VALUE(r_result) TYPE string.

    "! "Ermittelt" Studiendaten (generiert Beispielwerte).
    METHODS get_sponsor_patient_id
      IMPORTING
        i_patient_id    TYPE patnr
      RETURNING
        VALUE(r_result) TYPE string.

    "! "Ermittelt" Studiendaten (generiert Beispielwerte).
    METHODS supply_auth_provider
      IMPORTING
        i_case_data    TYPE nfal
        i_target_field TYPE REF TO if_ishmed_mci_hl7_field.

    "! "Ermittelt" Studiendaten (generiert Beispielwerte).
    METHODS get_auth_provider_id
      IMPORTING
        i_case_data     TYPE nfal ##needed
      RETURNING
        VALUE(r_result) TYPE gpartner.

ENDCLASS.



CLASS ZCL_MCIBUCH_HL7_CREATE_DEMO IMPLEMENTATION.


  METHOD create_csr_segment.

    " Segment erzeugen
    DATA(csr_segment) = NEW cl_ishmed_mci_hl7_segment(
        i_type        = 'CSR'
        i_field_count = 16
    ).

    " CSR-1 Sponsor Study ID
    csr_segment->if_ishmed_mci_hl7_segment~set_field_value(
        i_number = 1
        i_value  = get_study_id( )
    ).

    " CSR-4 Sponsor Patient ID
    csr_segment->if_ishmed_mci_hl7_segment~set_field_value(
        i_number = 4
        i_value = get_sponsor_patient_id( i_case_data-patnr )
    ).

    " CSR-6 Date/Time Of Patient Study Registration
    csr_segment->if_ishmed_mci_hl7_segment~set_field_value(
        i_number = 6
        i_value  = |{ sy-datum && sy-uzeit }|
    ).

    " CSR-8 Study Authorizing Provider --> XCN-Feld!
    DATA(auth_prov_field) = csr_segment->if_ishmed_mci_hl7_segment~get_field(
                                i_number = 8
                                i_create = abap_true
                            ).
    supply_auth_provider(
        i_case_data    = i_case_data
        i_target_field = auth_prov_field
    ).

    r_result = csr_segment.

  ENDMETHOD.


  METHOD create_message.

    " Falldaten lesen - brauchen wir an mehreren Stellen
    DATA case_data TYPE nfal.
    case_data = read_case_data(
        i_institution_id = i_institution_id
        i_case_id        = i_case_id
    ).

    " Struktur der Beispiel-Nachricht CRM_C01:
    " MSH
    " [  {  SFT  }  ]
    " {                    PATIENT
    "    PID
    "    [  PV1  ]
    "    CSR
    "    [  {  CSP  }  ]
    " }                    PATIENT

    " neue Nachricht anlegen
    r_result = NEW cl_ishmed_mci_hl7_message( ).
    DATA(segments) = r_result->get_segment_collection( ).

    " MSH-und SFT-Segment einfügen
    segments->add( create_msh_segment( ) ).
    segments->add( NEW cl_ishmed_mci_hl7_seg_sft_26( ) ).

    " PID- und PV1-Segment einfügen
    segments->add( NEW cl_ishmed_mci_hl7_seg_pid_26(
        i_einri = i_institution_id
        i_patnr = case_data-patnr
    ) ).
    segments->add( create_pv1_segment(
        i_case_data = case_data
    ) ).

    " CSR-Segment einfügen
    segments->add( create_csr_segment(
        i_case_data = case_data
    ) ).

  ENDMETHOD.


  METHOD create_msh_segment.

    " MSH-Segment mit Standardeinstellungen erzeugen
    DATA(msh_segment) = NEW cl_ishmed_mci_hl7_seg_msh_26(
        is_message_type  = VALUE #( message_code      = 'CRM'
                                    trigger_event     = 'C01'
                                    message_structure = 'CRM_C01' )
    ).

    " MSH-15 Accept Acknowledgment Type
    msh_segment->set_accept_acknowledgment_type( 'AL' ).

    " MSH-16 Application Acknowledgment Type
    msh_segment->set_appl_acknowledgment_type( 'NE' ).

    r_result = msh_segment.

  ENDMETHOD.


  METHOD create_pv1_segment.

    " Aufnahme bzw. erste Bewegung suchen
    DATA movement_data TYPE nbew.
    IF i_case_data-falar = '2'. " ambulant
      CALL FUNCTION 'ISH_NBEWTAB_GET_FIRST_VISIT'
        EXPORTING
          ss_einri = i_case_data-einri
          ss_falnr = i_case_data-falnr
        IMPORTING
          ss_nbew  = movement_data
        EXCEPTIONS
          OTHERS   = 1.
    ELSE.
      CALL FUNCTION 'ISH_NBEWTAB_GET_ADMISSION'
        EXPORTING
          ss_einri = i_case_data-einri
          ss_falnr = i_case_data-falnr
        IMPORTING
          ss_nbew  = movement_data
        EXCEPTIONS
          OTHERS   = 1.
    ENDIF.
    IF sy-subrc <> 0.
      FREE movement_data.
    ENDIF.

    " Segment mit Standardvorgaben erzeugen
    DATA(pv1_segment) = NEW cl_ishmed_mci_hl7_seg_pv1_26(
        i_einri  = i_case_data-einri
        i_falnr  = i_case_data-falnr
        i_lfdbew = movement_data-lfdnr
    ).

    r_result = pv1_segment.

  ENDMETHOD.


  METHOD execute.
    TRY.
        " HL7-Nachricht erzeugen und anzeigen, dann wieder verwerfen
        DATA(hl7_message) = create_message(
          i_institution_id = i_institution_id
          i_case_id        = i_case_id
        ).
        hl7_message->if_ishmed_mci_message~display( ).
        hl7_message->if_ishmed_mci_message~finalize( ).
      CATCH cx_ishmed_object
            cx_ishmed_mci
            cx_ishmed_illegal_position INTO DATA(mci_error).
        MESSAGE mci_error TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.


  METHOD get_auth_provider_id.
    r_result = '0000100003'.
  ENDMETHOD.


  METHOD get_sponsor_patient_id.
    r_result = reverse( i_patient_id ).
  ENDMETHOD.


  METHOD get_study_id.
    CONSTANTS co_minimum_random_study_id TYPE i VALUE 100000.
    CONSTANTS co_maximum_random_study_id TYPE i VALUE 999999.
    DATA(rng) = cl_abap_random=>create( ).
    r_result = |{ rng->intinrange(
                      low  = co_minimum_random_study_id
                      high = co_maximum_random_study_id
                  ) }|.
  ENDMETHOD.


  METHOD read_case_data.

    " Falldaten ermitteln
    CALL FUNCTION 'ISH_READ_NFAL'
      EXPORTING
        ss_einri = i_institution_id
        ss_falnr = i_case_id
      IMPORTING
        ss_nfal  = r_case_data
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      " Fehler beim Zugriff auf Fall &CASE_ID& in Einrichtung &INSTITUTION_ID&
      RAISE EXCEPTION TYPE zcx_mcibuch_mci_error
        EXPORTING
          textid         = zcx_mcibuch_mci_error=>case_read_error
          institution_id = i_institution_id
          case_id        = i_case_id.
    ENDIF.

  ENDMETHOD.


  METHOD supply_auth_provider.

    " Schlüssel des Arztes ermitteln
    DATA(provider_id) = get_auth_provider_id( i_case_data ).

    " Personendaten lesen
    DATA provider_data TYPE ngpa.
    CALL FUNCTION 'ISH_READ_NGPA'
      EXPORTING
        gpart  = provider_id
      IMPORTING
        ngpa_e = provider_data
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc = 0.

      " Zielfeld ist ein XCN-Feld

      " XCN-1 ID Number
      i_target_field->set_value(
          i_component_number = 1
          i_value            = provider_id
      ).

      " XCN-2 Family Name
      i_target_field->set_value(
          i_component_number = 1
          i_value            = provider_data-name1
      ).

      " XCN-3 Given Name
      i_target_field->set_value(
          i_component_number = 3
          i_value            = provider_data-name2
      ).

      " XCN-5 Suffix
      i_target_field->set_value(
          i_component_number = 5
          i_value            = provider_data-namzu
      ).

      " XCN-6 Prefix
      i_target_field->set_value(
          i_component_number = 6
          i_value            = provider_data-vorsw
      ).

      " XCN-7 Degree
      i_target_field->set_value(
          i_component_number = 7
          i_value            = provider_data-titel
      ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
