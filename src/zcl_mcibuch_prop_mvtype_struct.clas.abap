"! <strong>Eigenschaft Bewegungart (strukturiert)</strong>
"! <br/>
"! Diese Klasse stellt Eigenschaft zur Eingabe einer Bewegungsart über den
"! vollständigen Schlüssel (Einrichtung + Bewegungsart) in der Konfiguration einer
"! MCI-Komponente bereit.
"! <br/>
"! Diese Klasse ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 9.3.2
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
CLASS zcl_mcibuch_prop_mvtype_struct DEFINITION PUBLIC CREATE PUBLIC
  INHERITING FROM cl_ishmed_mci_property.

  PUBLIC SECTION.
    INTERFACES if_ishmed_mci_property_check.
    INTERFACES if_ishmed_mci_property_detail.

    CONSTANTS co_name TYPE n1mci_property_name VALUE 'ZMCIBUCH_MVTYPE_STRUCT'.
    CONSTANTS co_type TYPE typename VALUE 'ZMCIBUCH_BWART_KEY'.

    METHODS constructor
      IMPORTING
        i_default_value TYPE ri_bwart OPTIONAL
        i_mandatory     TYPE abap_bool DEFAULT abap_false
        i_category_name TYPE n1mci_category_name.

    METHODS if_ishmed_mci_property~get_label REDEFINITION.
    METHODS if_ishmed_mci_property~get_name REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS co_no_check TYPE spo_compcd VALUE 'NC'.
    CONSTANTS co_field_active TYPE spo_fattr VALUE '  '.
    CONSTANTS co_field_inactive TYPE spo_fattr VALUE '02'.
    CONSTANTS co_field_no_display TYPE spo_fattr VALUE '04'.

ENDCLASS.



CLASS zcl_mcibuch_prop_mvtype_struct IMPLEMENTATION.


  METHOD constructor.
    super->constructor(
        i_type          = co_type
        i_default_value = i_default_value
        i_mandatory     = i_mandatory
        i_category_name = i_category_name
    ).
  ENDMETHOD.


  METHOD if_ishmed_mci_property_check~execute.

    DATA current_value TYPE zmcibuch_bwart_key.
    me->get_value(
      IMPORTING
        e_value = current_value
    ).

    CALL FUNCTION 'ISH_BWART_CHECK'
      EXPORTING
        ss_einri = current_value-einri
        ss_bewty = '4' " ambulanter Besuch
        ss_bwart = current_value-bwart
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      ir_errorhandler->collect_messages(
          i_typ = 'E'
          i_kla = sy-msgid
          i_num = sy-msgno
          i_mv1 = sy-msgv1
          i_mv2 = sy-msgv2
          i_mv3 = sy-msgv3
          i_mv4 = sy-msgv4
      ).
    ENDIF.

  ENDMETHOD.


  METHOD if_ishmed_mci_property_detail~show.

    " Anzeigemodus berücksichtigen
    DATA(display_only) = xsdbool(
      i_vcode = if_ish_constant_definition=>co_vcode_display ).
    IF display_only = abap_true.
      DATA(field_setting) = co_field_inactive.
    ELSE.
      field_setting = co_field_active.
    ENDIF.

    " aktuellen Wert beschaffen
    DATA current_value TYPE zmcibuch_bwart_key.
    me->get_value(
      IMPORTING
        e_value = current_value
    ).

    " Feldtabelle aufbauen
    DATA(popup_fields) = VALUE rn1mci_sval_t(
      " Eingabefeld Einrichtung
      ( tabname    = 'NBEW'
        fieldname  = 'EINRI'
        field_attr = field_setting
        value      = current_value-einri
        field_obl  = abap_true
        comp_code  = co_no_check )
      " Prüfung Einrichtung
      ( tabname    = 'TN14B'
        fieldname  = 'EINRI'
        field_attr = co_field_no_display
        comp_tab   = 'NBEW'
        comp_field = 'EINRI' )

      " Bewegungtyp fix 4, deshalb nicht anzeigen
      ( tabname    = 'NBEW'
        fieldname  = 'BEWTY'
        value      = '4'  " ambulanter Besuch
        field_attr = co_field_inactive
        field_obl  = abap_true
        comp_code  = co_no_check )
      " Prüfung Bewegungstyp
      ( tabname    = 'TN14B'
        fieldname  = 'BEWTY'
        field_attr = co_field_no_display
        comp_tab   = 'NBEW'
        comp_field = 'BEWTY' )

      " Eingabefeld Bewegungsart
      ( tabname    = 'NBEW'
        fieldname  = 'BWART'
        field_attr = field_setting
        value      = current_value-bwart
        field_obl  = abap_true
        comp_code  = co_no_check )
      " Prüfung Bewegungsart
      ( tabname    = 'TN14B'
        fieldname  = 'BWART'
        field_attr = co_field_no_display
        comp_tab   = 'NBEW'
        comp_field = 'BWART' )
    ).

    " Popup anzeigen
    DATA return_code TYPE c LENGTH 1.
    CALL FUNCTION 'POPUP_GET_VALUES_DB_CHECKED'
      EXPORTING
        popup_title = 'Bewegungsart'(002)
      IMPORTING
        returncode  = return_code
      TABLES
        fields      = popup_fields.

    " Wert zurückübertragen
    IF return_code <> 'A' AND display_only = abap_false.
      LOOP AT popup_fields ASSIGNING FIELD-SYMBOL(<field>) WHERE tabname = 'NBEW'.
        CASE <field>-fieldname.
          WHEN 'EINRI'.
            current_value-einri = <field>-value.        "#EC CI_CONV_OK
          WHEN 'BWART'.
            current_value-bwart = <field>-value.        "#EC CI_CONV_OK
        ENDCASE.
      ENDLOOP.
      me->set_value( current_value ).
    ENDIF.

  ENDMETHOD.


  METHOD if_ishmed_mci_property~get_label.
    r_value = 'Bewegungsart (strukturiert)'(001).
  ENDMETHOD.


  METHOD if_ishmed_mci_property~get_name.
    r_value = co_name.
  ENDMETHOD.
ENDCLASS.
