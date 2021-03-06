"! <strong>Hüllklasse zur Ansteuerung von CL_ISHMED_TPI_HISTORY_API</strong>
"! <br/>
"! Diese Klasse stellt eine Kapsel um die HISTORY-Implementierung dar. Durch die
"! Verwendung des zwischengeschalteten Interface ZIF_MCIBUCH_HISTORY_API kann die
"! eigentliche Funktion zu Testzwecken simuliert werden.
"! <br/>
"! Diese Klasse ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 8.3.2
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
CLASS zcl_mcibuch_wrapper_history DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcibuch_history_api.

    METHODS constructor
      IMPORTING
        i_process_name    TYPE n1mci_process_name
        i_external_key    TYPE n1mci_external_key
        i_object_category TYPE swf_clstyp
        i_object_type     TYPE sibftypeid
      RAISING
        cx_ishmed_locked .

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA wrapped_api_instance TYPE REF TO cl_ishmed_tpi_history_api.


ENDCLASS.



CLASS ZCL_MCIBUCH_WRAPPER_HISTORY IMPLEMENTATION.


  METHOD constructor.
    me->wrapped_api_instance = cl_ishmed_tpi_history_api=>get_instance(
        i_process_name    = i_process_name
        i_external_key    = i_external_key
        i_object_category = i_object_category
        i_object_type     = i_object_type
    ).
  ENDMETHOD.


  METHOD if_ishmed_object~finalize.
    me->wrapped_api_instance->if_ishmed_object~finalize( ).
  ENDMETHOD.


  METHOD zif_mcibuch_history_api~get_external_key.
    r_value = me->wrapped_api_instance->get_external_key( ).
  ENDMETHOD.


  METHOD zif_mcibuch_history_api~get_message_date.
    r_value = me->wrapped_api_instance->get_message_date( ).
  ENDMETHOD.


  METHOD zif_mcibuch_history_api~get_message_time.
    r_value = me->wrapped_api_instance->get_message_time( ).
  ENDMETHOD.


  METHOD zif_mcibuch_history_api~get_object_category.
    r_value = me->wrapped_api_instance->get_object_category( ).
  ENDMETHOD.


  METHOD zif_mcibuch_history_api~get_object_type.
    r_value = me->wrapped_api_instance->get_object_type( ).
  ENDMETHOD.


  METHOD zif_mcibuch_history_api~get_process_name.
    r_value = me->wrapped_api_instance->get_process_name( ).
  ENDMETHOD.


  METHOD zif_mcibuch_history_api~save.
    me->wrapped_api_instance->save( ).
  ENDMETHOD.


  METHOD zif_mcibuch_history_api~set_message_date.
    me->wrapped_api_instance->set_message_date( i_value ).
  ENDMETHOD.


  METHOD zif_mcibuch_history_api~set_message_time.
    me->wrapped_api_instance->set_message_time( i_value ).
  ENDMETHOD.
ENDCLASS.
