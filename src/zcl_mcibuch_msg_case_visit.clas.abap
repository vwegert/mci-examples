"! <strong>Beispiel-Nachrichtentyp für BAPI_PATCASE_ADDOUTPATVISIT</strong>
"! <br/>
"! Diese Klasse ist nicht separat testbar, sondern zeigt lediglich die
"! Vorgehensweise zur Erstellung einer Nachrichtenklasse.
"! <br/>
"! Diese Klasse ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 5.3
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
CLASS zcl_mcibuch_msg_case_visit DEFINITION PUBLIC CREATE PUBLIC
  INHERITING FROM cl_ishmed_mci_abap_dt_message.

  PUBLIC SECTION.

    "! Feldname: Einrichtung
    CONSTANTS co_field_institution TYPE deffdname VALUE 'EINRI'.

    "! Feldname: Fallnummer
    CONSTANTS co_field_case_id TYPE deffdname VALUE 'FALNR'.

    "! Feldname: Stornokennzeichen
    CONSTANTS co_field_cancel_flag TYPE deffdname VALUE 'CANCEL_FLAG'.

    "! Feldname: Bewegungsdaten administrativ
    CONSTANTS co_field_visit_data_adm TYPE deffdname VALUE 'OUTPAT_VISIT_DATA'.

    "! Feldname: Bewegungsdaten medizinisch
    CONSTANTS co_field_visit_data_med TYPE deffdname VALUE 'OUTPAT_VISIT_DATA_MED'.

    "! Feldname: Zusatzdaten Landesversion Österreich
    CONSTANTS co_field_visit_data_at TYPE deffdname VALUE 'OUTPAT_VISIT_DATA_AT'.

    "! Ermittelt die Einrichtung.
    METHODS get_institution
      RETURNING
        VALUE(r_result) TYPE einri.

    "! Setzt die Einrichtung.
    METHODS set_institution
      IMPORTING
        i_value TYPE einri.

    "! Ermittelt die Fallnummer.
    METHODS get_case_id
      RETURNING
        VALUE(r_result) TYPE falnr.

    "! Setzt die Fallnummer.
    METHODS set_case_id
      IMPORTING
        i_value TYPE falnr.

    "! Ermittelt das Stornokennzeichen.
    METHODS is_cancelled
      RETURNING
        VALUE(r_result) TYPE ri_storn.

    "! Setzt das Stornokennzeichen.
    METHODS set_cancelled
      IMPORTING
        i_value TYPE ri_storn.

    "! Ermittelt die administrativen Bewegungsdaten.
    METHODS get_visit_data_adm
      RETURNING
        VALUE(r_result) TYPE bapi2097ovin.

    "! Setzt die administrativen Bewegungsdaten.
    METHODS set_visit_data_adm
      IMPORTING
        i_value TYPE bapi2097ovin.

    "! Ermittelt die medizinischen Bewegungsdaten.
    METHODS get_visit_data_med
      RETURNING
        VALUE(r_result) TYPE bapi2097n1med.

    "! Setzt die medizinischen Bewegungsdaten.
    METHODS set_visit_data_med
      IMPORTING
        i_value TYPE bapi2097n1med.

    "! Ermittelt die Zusatzdaten Landesversion Österreich.
    METHODS get_visit_data_at
      RETURNING
        VALUE(r_result) TYPE bapi2097atin.

    "! Setzt die Zusatzdaten Landesversion Österreich.
    METHODS set_visit_data_at
      IMPORTING
        i_value TYPE bapi2097atin.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_MCIBUCH_MSG_CASE_VISIT IMPLEMENTATION.


  METHOD get_case_id.
    __get_value__ co_field_case_id.
  ENDMETHOD.


  METHOD get_institution.
    __get_value__ co_field_institution.
  ENDMETHOD.


  METHOD get_visit_data_adm.
    __get_value__ co_field_visit_data_adm.
  ENDMETHOD.


  METHOD get_visit_data_at.
    __get_value__ co_field_visit_data_at.
  ENDMETHOD.


  METHOD get_visit_data_med.
    __get_value__ co_field_visit_data_med.
  ENDMETHOD.


  METHOD is_cancelled.
    __get_value__ co_field_cancel_flag.
  ENDMETHOD.


  METHOD set_cancelled.
    __set_value__ co_field_cancel_flag.
  ENDMETHOD.


  METHOD set_case_id.
    __set_value__ co_field_case_id.
  ENDMETHOD.


  METHOD set_institution.
    __set_value__ co_field_institution.
  ENDMETHOD.


  METHOD set_visit_data_adm.
    __set_value__ co_field_visit_data_adm.
  ENDMETHOD.


  METHOD set_visit_data_at.
    __set_value__ co_field_visit_data_at.
  ENDMETHOD.


  METHOD set_visit_data_med.
    __set_value__ co_field_visit_data_med.
  ENDMETHOD.
ENDCLASS.
