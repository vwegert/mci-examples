"! <strong>Beispiel-Konfiguration</strong>
"! <br/>
"! Diese Klasse demonstriert die Bereitstellung einer Konfigurationsklasse.
"! Die Verwendung erfolgt in der Beispielklasse ZCL_MCIBUCH_TRAFO_CONFDEMO.
"! <br/>
"! Diese Klasse ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 9.2
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
CLASS zcl_mcibuch_trafo_confdemo_cfg DEFINITION PUBLIC CREATE PUBLIC
  INHERITING FROM cl_ishmed_mci_configuration.

  PUBLIC SECTION.
    "! Schlüssel der Kategorie "Allgemein"
    CONSTANTS co_category_general TYPE n1mci_category_name VALUE 'GENERAL'.

    "! Schlüssel der Kategorie "System"
    CONSTANTS co_category_system  TYPE n1mci_category_name VALUE 'SYSTEM'.

    "! Schlüssel der Kategorie "Benutzer"
    CONSTANTS co_category_user    TYPE n1mci_category_name VALUE 'USER'.

    "! Schlüssel der Kategorie "Eigenentwicklung"
    CONSTANTS co_category_custom  TYPE n1mci_category_name VALUE 'CUSTOM'.

    METHODS constructor.
    METHODS if_ishmed_mci_configuration~get_category_list REDEFINITION.
    METHODS if_ishmed_mci_configuration~check REDEFINITION.

    METHODS get_institution
      RETURNING
        VALUE(r_result) TYPE einri.

    METHODS get_documenting_org_unit
      RETURNING
        VALUE(r_result) TYPE orgdo.

    METHODS get_number_object
      RETURNING
        VALUE(r_result) TYPE nrobj.

    METHODS get_number_range
      RETURNING
        VALUE(r_result) TYPE nrnr.

    METHODS get_resp_employee
      RETURNING
        VALUE(r_result) TYPE n1mitarb.

    METHODS get_notify_flag
      RETURNING
        VALUE(r_result) TYPE n1mci_notification_active.

  PROTECTED SECTION.

  PRIVATE SECTION.

    "! Zugriff auf eine Eigenschaft mit Fehlerbehandlung
    METHODS get_checked_property_value
      IMPORTING
        i_property_name TYPE n1mci_property_name
      EXPORTING
        e_value         TYPE any.

    "! Prüft die Zuordnung des Mitarbeiters zur dokumentierenden OE.
    METHODS check_doc_ou_empl_assignment
      IMPORTING
        ir_errorhandler TYPE REF TO cl_ishmed_errorhandling.

ENDCLASS.



CLASS ZCL_MCIBUCH_TRAFO_CONFDEMO_CFG IMPLEMENTATION.


  METHOD check_doc_ou_empl_assignment.
    DATA(institution) = me->get_institution( ).
    DATA(org_unit) = me->get_documenting_org_unit( ).
    DATA(employee) = me->get_resp_employee( ).
    IF institution IS NOT INITIAL AND
       org_unit    IS NOT INITIAL AND
       employee    IS NOT INITIAL.
      CALL FUNCTION 'ISHMED_CHECK_NGPA' " siehe Dokumentation!
        EXPORTING
          ss_einri    = institution
          ss_orgid    = org_unit
          ss_pernr    = employee
          ss_chk_auth = abap_false " keine Berechtigungsprüfung
        EXCEPTIONS
          OTHERS      = 4.
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
    ENDIF.
  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    " --- Allgemein ---------------------------------------
    me->add_property(
        NEW cl_ishmed_mci_institution(
            i_category_name = co_category_general
            i_mandatory     = abap_true
        )
    ).
    me->add_property(
        NEW cl_ishmed_mci_documenting_ou(
            i_category_name = co_category_general
            i_mandatory     = abap_true
        )
    ).

    " --- System ------------------------------------------
    me->add_property_group(
        NEW cl_ishmed_mci_pgroup_number(
            i_category_name = co_category_system
        )
    ).

    " --- Benutzer ----------------------------------------
    me->add_property(
        NEW cl_ishmed_mci_resp_employee(
            i_category_name = co_category_user
        )
    ).
    me->add_property(
        NEW cl_ishmed_mci_notify_flag(
            i_default_value = abap_true
            i_category_name = co_category_user
        )
    ).

    " --- Eigenentwicklung --------------------------------
    me->add_property(
        NEW zcl_mcibuch_prop_mvtype_simple(
            i_category_name = co_category_custom
        )
    ).
    me->add_property(
        NEW zcl_mcibuch_prop_mvtype_struct(
            i_category_name = co_category_custom
        )
    ).

  ENDMETHOD.


  METHOD get_checked_property_value.
    TRY.
        DATA(property) = me->get_property( i_property_name ).
        property->get_value(
          IMPORTING
            e_value = e_value
        ).
      CATCH cx_ishmed_mci_configuration cx_sy_move_cast_error INTO DATA(exception).
        " Fehler beim Zugriff auf die Eigenschaft &PROPERTY_NAME& der Konfiguration
        RAISE EXCEPTION TYPE zcx_mcibuch_dynamic_check
          EXPORTING
            textid        = zcx_mcibuch_dynamic_check=>property_error
            previous      = exception
            property_name = i_property_name.
    ENDTRY.
  ENDMETHOD.


  METHOD get_documenting_org_unit.
    get_checked_property_value(
      EXPORTING
        i_property_name = cl_ishmed_mci_documenting_ou=>co_name
      IMPORTING
        e_value         = r_result
    ).
  ENDMETHOD.


  METHOD get_institution.
    get_checked_property_value(
      EXPORTING
        i_property_name = cl_ishmed_mci_institution=>co_name
      IMPORTING
        e_value         = r_result
    ).
  ENDMETHOD.


  METHOD get_notify_flag.
    get_checked_property_value(
      EXPORTING
        i_property_name = cl_ishmed_mci_notify_flag=>co_name
      IMPORTING
        e_value         = r_result
    ).
  ENDMETHOD.


  METHOD get_number_object.
    get_checked_property_value(
      EXPORTING
        i_property_name = cl_ishmed_mci_number_object=>co_name
      IMPORTING
        e_value         = r_result
    ).
  ENDMETHOD.


  METHOD get_number_range.
    get_checked_property_value(
      EXPORTING
        i_property_name = cl_ishmed_mci_number_range=>co_name
      IMPORTING
        e_value         = r_result
    ).
  ENDMETHOD.


  METHOD get_resp_employee.
    get_checked_property_value(
      EXPORTING
        i_property_name = cl_ishmed_mci_resp_employee=>co_name
      IMPORTING
        e_value         = r_result
    ).
  ENDMETHOD.


  METHOD if_ishmed_mci_configuration~check.
    super->check( ir_errorhandler ).
    check_doc_ou_empl_assignment( ir_errorhandler ).
  ENDMETHOD.


  METHOD if_ishmed_mci_configuration~get_category_list.
    rt_value = VALUE #(
      ( name = co_category_general text = 'Allgemein'(001) )
      ( name = co_category_system  text = 'System'(002) )
      ( name = co_category_user    text = 'Benutzer'(003) )
      ( name = co_category_custom  text = 'Eigenentwicklung'(004) )
    ).
  ENDMETHOD.
ENDCLASS.
