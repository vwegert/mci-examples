"! Diese Klasse ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
CLASS zcx_mcibuch_mci_error DEFINITION PUBLIC FINAL CREATE PUBLIC
  INHERITING FROM cx_ishmed_mci.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF zcx_mcibuch_mci_error,
        msgid TYPE symsgid VALUE 'ZMCIBUCH',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_mcibuch_mci_error.
    CONSTANTS:
      BEGIN OF message_type_not_hl7,
        msgid TYPE symsgid VALUE 'ZMCIBUCH',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF message_type_not_hl7.
    CONSTANTS:
      BEGIN OF unsupported_hl7_version,
        msgid TYPE symsgid VALUE 'ZMCIBUCH',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'ACTUAL_VERSION',
        attr2 TYPE scx_attrname VALUE 'EXPECTED_MIN_VERSION',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF unsupported_hl7_version.
    CONSTANTS:
      BEGIN OF unsupported_msg_struct,
        msgid TYPE symsgid VALUE 'ZMCIBUCH',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'ACTUAL_MSG_STRUCTURE',
        attr2 TYPE scx_attrname VALUE 'EXPECTED_MSG_STRUCTURE',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF unsupported_msg_struct.
    CONSTANTS:
      BEGIN OF case_read_error,
        msgid TYPE symsgid VALUE 'ZMCIBUCH',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'CASE_ID',
        attr2 TYPE scx_attrname VALUE 'INSTITUTION_ID',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF case_read_error.
    CONSTANTS:
      BEGIN OF institution_or_case_missing,
        msgid TYPE symsgid VALUE 'ZMCIBUCH',
        msgno TYPE symsgno VALUE '012',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF institution_or_case_missing.
    CONSTANTS:
      BEGIN OF txa_segment_missing,
        msgid TYPE symsgid VALUE 'ZMCIBUCH',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF txa_segment_missing.
    CONSTANTS:
      BEGIN OF context_error,
        msgid TYPE symsgid VALUE 'ZMCIBUCH',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF context_error.
    CONSTANTS:
      BEGIN OF content_mapping_error,
        msgid TYPE symsgid VALUE 'ZMCIBUCH',
        msgno TYPE symsgno VALUE '015',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF content_mapping_error.
    CONSTANTS:
      BEGIN OF message_type_not_addvisit,
        msgid TYPE symsgid VALUE 'ZMCIBUCH',
        msgno TYPE symsgno VALUE '020',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF message_type_not_addvisit.
    CONSTANTS:
      BEGIN OF external_key_missing,
        msgid TYPE symsgid VALUE 'ZMCIBUCH',
        msgno TYPE symsgno VALUE '028',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF external_key_missing.
    CONSTANTS:
      BEGIN OF table_api_locked,
        msgid TYPE symsgid VALUE 'ZMCIBUCH',
        msgno TYPE symsgno VALUE '029',
        attr1 TYPE scx_attrname VALUE 'TABLE_NAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF table_api_locked.
    CONSTANTS:
      BEGIN OF date_time_missing,
        msgid TYPE symsgid VALUE 'ZMCIBUCH',
        msgno TYPE symsgno VALUE '030',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF date_time_missing.
    CONSTANTS:
      BEGIN OF movement_read_error,
        msgid TYPE symsgid VALUE 'ZMCIBUCH',
        msgno TYPE symsgno VALUE '032',
        attr1 TYPE scx_attrname VALUE 'MOVEMENT_NUMBER',
        attr2 TYPE scx_attrname VALUE 'CASE_ID',
        attr3 TYPE scx_attrname VALUE 'INSTITUTION_ID',
        attr4 TYPE scx_attrname VALUE '',
      END OF movement_read_error.
    CONSTANTS:
      BEGIN OF movement_data_inconsistent,
        msgid TYPE symsgid VALUE 'ZMCIBUCH',
        msgno TYPE symsgno VALUE '035',
        attr1 TYPE scx_attrname VALUE 'EXTERNAL_KEY',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF movement_data_inconsistent.
    DATA expected_min_version TYPE n1mci_hl7_version_id.
    DATA actual_version TYPE n1mci_hl7_version_id.
    DATA expected_msg_structure TYPE n1mci_hl7_message_structure.
    DATA actual_msg_structure TYPE n1mci_hl7_message_structure.
    DATA institution_id TYPE einri.
    DATA case_id TYPE falnr.
    DATA table_name TYPE tabname.
    DATA movement_number TYPE lfdbew.
    DATA external_key TYPE n1mci_external_key.

    METHODS constructor
      IMPORTING
        !textid                 LIKE if_t100_message=>t100key OPTIONAL
        !previous               LIKE previous OPTIONAL
        !gr_errorhandler        TYPE REF TO cl_ishmed_errorhandling OPTIONAL
        !gr_msgtyp              TYPE sy-msgty DEFAULT 'E'
        !attr1                  TYPE symsgv OPTIONAL
        !attr2                  TYPE symsgv OPTIONAL
        !attr3                  TYPE symsgv OPTIONAL
        !attr4                  TYPE symsgv OPTIONAL
        !expected_min_version   TYPE n1mci_hl7_version_id OPTIONAL
        !actual_version         TYPE n1mci_hl7_version_id OPTIONAL
        !expected_msg_structure TYPE n1mci_hl7_message_structure OPTIONAL
        !actual_msg_structure   TYPE n1mci_hl7_message_structure OPTIONAL
        !institution_id         TYPE einri OPTIONAL
        !case_id                TYPE falnr OPTIONAL
        !table_name             TYPE tabname OPTIONAL
        !movement_number        TYPE lfdbew OPTIONAL
        !external_key           TYPE n1mci_external_key OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCX_MCIBUCH_MCI_ERROR IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous        = previous
        gr_errorhandler = gr_errorhandler
        gr_msgtyp       = gr_msgtyp
        attr1           = attr1
        attr2           = attr2
        attr3           = attr3
        attr4           = attr4.
    me->expected_min_version = expected_min_version.
    me->actual_version = actual_version.
    me->expected_msg_structure = expected_msg_structure.
    me->actual_msg_structure = actual_msg_structure.
    me->institution_id = institution_id.
    me->case_id = case_id.
    me->table_name = table_name.
    me->movement_number = movement_number.
    me->external_key = external_key.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = zcx_mcibuch_mci_error.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
