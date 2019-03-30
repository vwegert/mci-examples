"! <strong>Schnittstelle für BAPI-Aufrufe</strong>
"! <br/>
"! Dieses Interface stellt Methoden bereit, die IS-H-BAPI-Funktionsbausteinaufrufe
"! abbilden. Dadurch ist es möglich, eine Implementierungsklasse zu Testzwecken
"! durch generierte Test Doubles zu ersetzen.
"! <br/>
"! Dieses Interface ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 8.3.2
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
INTERFACE zif_mcibuch_visit_bapi_wrapper PUBLIC.

  "! Aufruf des Funktionsbausteins BAPI_PATCASE_GETOUTPATVISIT.
  METHODS call_patcase_getoutpatvisit
    IMPORTING
      i_institution          TYPE einri
      i_case_id              TYPE falnr
      i_movement_number      TYPE lfdbew
    EXPORTING
      es_visit_data          TYPE bapi2097ovout
      es_visit_data_at       TYPE bapi2097atout
      e_worst_returned_msgty TYPE ish_bapiretmaxty
      et_messages            TYPE ishmed_t_bapiret2.

  "! Aufruf des Funktionsbausteins BAPI_PATCASE_ADDOUTPATVISIT
  METHODS call_patcase_addoutpatvisit
    IMPORTING
      i_institution            TYPE einri
      i_case_id                TYPE falnr
      is_outpat_visit_data     TYPE bapi2097ovin
      is_outpat_visit_data_med TYPE bapi2097n1med OPTIONAL
      is_outpat_visit_data_at  TYPE bapi2097atin OPTIONAL
      i_testrun                TYPE abap_bool
    EXPORTING
      es_new_case_data         TYPE bapi2097case
      es_new_movement_data     TYPE bapi2097move
      es_new_movement_data_at  TYPE bapi2097moveat
      e_worst_returned_msgty   TYPE ish_bapiretmaxty
      et_messages              TYPE ishmed_t_bapiret2.

  "! Aufruf des Funktionsbausteins BAPI_PATCASE_CHANGEOUTPATVISIT
  METHODS call_patcase_changeoutpatvisit
    IMPORTING
      i_institution               TYPE einri
      i_case_id                   TYPE falnr
      i_movement_number           TYPE lfdbew
      is_changed_visit_data       TYPE bapi2097ovinx
      is_changed_visit_data_med   TYPE bapi2097n1medx OPTIONAL
      is_changed_visit_data_at    TYPE bapi2097atinx OPTIONAL
      i_testrun                   TYPE abap_bool
    EXPORTING
      es_changed_case_data        TYPE bapi2097case
      es_changed_movement_data    TYPE bapi2097move
      es_changed_movement_data_at TYPE bapi2097moveat
      e_worst_returned_msgty      TYPE ish_bapiretmaxty
      et_messages                 TYPE ishmed_t_bapiret2.

  "! Aufruf des Funktionsbausteins BAPI_PATCASE_CANCELOUTPATVISIT
  METHODS call_patcase_canceloutpatvisit
    IMPORTING
      i_institution          TYPE einri
      i_case_id              TYPE falnr
      i_movement_number      TYPE lfdbew
      is_cancel_user_date    TYPE bapincancelmove OPTIONAL
      i_testrun              TYPE abap_bool
    EXPORTING
      e_worst_returned_msgty TYPE ish_bapiretmaxty
      et_messages            TYPE ishmed_t_bapiret2.

  "! Aufruf des Funktionsbausteins BAPI_TRANSACTION_COMMIT
  METHODS call_transaction_commit
    IMPORTING
      i_wait     TYPE bapiwait OPTIONAL
    EXPORTING
      es_message TYPE bapiret2.

  "! Aufruf des Funktionsbausteins BAPI_TRANSACTION_ROLLBACK
  METHODS call_transaction_rollback
    EXPORTING
      es_message TYPE bapiret2.

ENDINTERFACE.
