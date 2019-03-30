"! <strong>Eigenschaft Bewegungart (einfach)</strong>
"! <br/>
"! Diese Klasse stellt Eigenschaft zur isolierten Eingabe einer Bewegungsart
"! in der Konfiguration einer MCI-Komponente bereit.
"! <br/>
"! Diese Klasse ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 9.3.2
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
CLASS zcl_mcibuch_prop_mvtype_simple DEFINITION PUBLIC CREATE PUBLIC
  INHERITING FROM cl_ishmed_mci_property.

  PUBLIC SECTION.

    INTERFACES if_ishmed_mci_property_f4.

    CONSTANTS co_name TYPE n1mci_property_name VALUE 'ZMCIBUCH_MVTYPE_SIMPLE'.
    CONSTANTS co_type TYPE typename VALUE 'RI_BWART'.

    METHODS constructor
      IMPORTING
        i_default_value TYPE ri_bwart OPTIONAL
        i_mandatory     TYPE abap_bool DEFAULT abap_false
        i_category_name TYPE n1mci_category_name.

    METHODS if_ishmed_mci_property~get_label REDEFINITION.
    METHODS if_ishmed_mci_property~get_name REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_MCIBUCH_PROP_MVTYPE_SIMPLE IMPLEMENTATION.


  METHOD constructor.
    super->constructor(
        i_type          = co_type
        i_default_value = i_default_value
        i_mandatory     = i_mandatory
        i_category_name = i_category_name
    ).
  ENDMETHOD.


  METHOD if_ishmed_mci_property_f4~show.

    DATA(display_only) = xsdbool(
      i_vcode = if_ish_constant_definition=>co_vcode_display ).

    " Beschreibung der Suchhilfe ermitteln
    DATA search_help_desc TYPE shlp_descr.
    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        shlpname = 'H_TN14B'
      IMPORTING
        shlp     = search_help_desc.

    " Selektionsparameter zur Werteeinschränkung setzen
    search_help_desc-selopt = VALUE #( (
        shlpname  = 'H_TN14B'
        shlpfield = 'BEWTY'
        sign      = 'I'
        option    = 'EQ'
        low       = '4' " ambulanter Besuch
    ) ).

    " Ergebnisfeld markieren
    LOOP AT search_help_desc-interface ASSIGNING FIELD-SYMBOL(<interface>).
      CASE <interface>-shlpfield.
        WHEN 'BWART'.
          <interface>-valfield = 'BWART'.
      ENDCASE.
    ENDLOOP.

    " Suchhilfe aufrufen
    DATA return_code TYPE sysubrc.
    DATA return_values TYPE STANDARD TABLE OF ddshretval.
    CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
      EXPORTING
        shlp          = search_help_desc
        disponly      = display_only
      IMPORTING
        rc            = return_code
      TABLES
        return_values = return_values.

    " Wert zurückschreiben
    IF return_code = 0 AND display_only = abap_false.
      ASSERT lines( return_values ) = 1.
      DATA(new_value) = CONV ri_bwart( return_values[ 1 ]-fieldval ). "#EC CI_CONV_OK
      me->set_value( i_value = new_value ).
    ENDIF.

  ENDMETHOD.


  METHOD if_ishmed_mci_property~get_label.
    r_value = 'Bewegungsart (einfach)'(001).
  ENDMETHOD.


  METHOD if_ishmed_mci_property~get_name.
    r_value = co_name.
  ENDMETHOD.
ENDCLASS.
