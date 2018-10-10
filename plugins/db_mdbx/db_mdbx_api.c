#include "ucm.h"
#include "db_mdbx_api.h"

ucm_dbval_t*
(*get_dbcontact_data) ( ucm_contact_t* contact,
                        ucm_plugin_t*  module,
                        char*          setting,
                        ucm_dbval_t*   defVal);

UCM_RET
(*set_dbcontact_data) ( ucm_contact_t* contact,
                        ucm_plugin_t*  module,
                        char*          setting,
                        ucm_dbval_t*   value);

