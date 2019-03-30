"! <strong>Datensatz aus einer CSV-Datei</strong>
"! <br/>
"! Diese Klasse dient als emittierter Nachrichtentyp des Beispiel-Start-Konnektors
"! ZCL_MCIBUCH_STARTCONN_CSV_FILE.
"! <br/>
"! Diese Klasse ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 7.3.1
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
CLASS zcl_mcibuch_msg_csv_dataset DEFINITION PUBLIC CREATE PUBLIC
  INHERITING FROM cl_ishmed_mci_file_message.

  PUBLIC SECTION.
    "! Erzeugt eine neue Nachricht mit den übergebenen Daten.
    "! @parameter is_file | Angaben zur Quelldatei
    "! @parameter i_line | Zeilennummer
    "! @parameter i_data | Inhalt der Zeile
    METHODS constructor
      IMPORTING
        is_file TYPE rsfillst
        i_line  TYPE i
        i_data  TYPE string.

    "! Ermittelt die Zeilennummer des Datensatzes in der CSV-Datei.
    "! Die Kopfzeile mit den Feldnamen wird bei der Zählung
    "! <strong>nicht</strong> mitgezählt.
    METHODS get_line
      RETURNING
        VALUE(r_result) TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.
    "! Zeilennummer in der CSV-Datei
    DATA line TYPE i.

ENDCLASS.



CLASS ZCL_MCIBUCH_MSG_CSV_DATASET IMPLEMENTATION.


  METHOD constructor.
    super->constructor(
        i_data  = i_data
        is_file = is_file
    ).
    me->line = i_line.
  ENDMETHOD.


  METHOD get_line.
    r_result = me->line.
  ENDMETHOD.
ENDCLASS.
