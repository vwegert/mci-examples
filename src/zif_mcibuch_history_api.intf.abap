"! <strong>Schnittstelle zur Klasse CL_ISHMED_TPI_HISTORY_API</strong>
"! <br/>
"! Dieses Interface stellt Methoden bereit, die die Funktionen der HISTORY-API
"! abbilden. Darüber kann die eigentliche Funktion zu Testzwecken simuliert werden.
"! <br/>
"! Dieses Interface ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 8.3.2
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
INTERFACE zif_mcibuch_history_api PUBLIC.

  INTERFACES if_ishmed_object.

  "! Siehe Klasse CL_ISHMED_TPI_HISTORY_API Methode GET_EXTERNAL_KEY.
  METHODS get_external_key
    RETURNING
      VALUE(r_value) TYPE n1mci_external_key.

  "! Siehe Klasse CL_ISHMED_TPI_HISTORY_API Methode GET_MESSAGE_DATE.
  METHODS get_message_date
    RETURNING
      VALUE(r_value) TYPE n1mci_message_date.

  "! Siehe Klasse CL_ISHMED_TPI_HISTORY_API Methode GET_MESSAGE_TIME.
  METHODS get_message_time
    RETURNING
      VALUE(r_value) TYPE n1mci_message_time.

  "! Siehe Klasse CL_ISHMED_TPI_HISTORY_API Methode GET_OBJECT_CATEGORY.
  METHODS get_object_category
    RETURNING
      VALUE(r_value) TYPE swf_clstyp.

  "! Siehe Klasse CL_ISHMED_TPI_HISTORY_API Methode GET_OBJECT_TYPE.
  METHODS get_object_type
    RETURNING
      VALUE(r_value) TYPE sibftypeid.

  "! Siehe Klasse CL_ISHMED_TPI_HISTORY_API Methode GET_PROCESS_NAME.
  METHODS get_process_name
    RETURNING
      VALUE(r_value) TYPE n1mci_process_name.

  "! Siehe Klasse CL_ISHMED_TPI_HISTORY_API Methode SAVE.
  METHODS save.

  "! Siehe Klasse CL_ISHMED_TPI_HISTORY_API Methode SET_MESSAGE_DATE.
  METHODS set_message_date
    IMPORTING
      i_value TYPE n1mci_message_date.

  "! Siehe Klasse CL_ISHMED_TPI_HISTORY_API Methode set_message_time.
  METHODS set_message_time
    IMPORTING
      i_value TYPE n1mci_message_time.

ENDINTERFACE.
