#pragma once

/*! Enums what defines the plugin area */
enum
{
    UCM_TYPE_PLUG_DB     = 1,
    UCM_TYPE_PLUG_PROTO  = 2,
    UCM_TYPE_PLUG_CRYPTO = 3,
    UCM_TYPE_PLUG_HIST   = 4,
    UCM_TYPE_PLUG_GUI    = 5,
    UCM_TYPE_PLUG_STUFF  = 6
};

/*! Usage API version */
typedef struct
{
    const uint8_t vmajor;
    const uint8_t vminor;
} ucm_vapi_t;

enum
{
    UCM_FLAG_PLUG_LOGGED = 1,
};

typedef struct
{
    const ucm_vapi_t api;  /// plugin release api version (required)
    const uint8_t sys;     /// plugin subsystem (required)
    const uint16_t vmajor; /// major plugin version (required)
    const uint16_t vminor; /// minor plugin version (required)
    const uint16_t vpatch; /// patch plugin version (required)
    uint32_t flags;        /// plugin flags
    // build info.
    struct
    {
        const wchar_t* commit;   /// commit in repository
        const wchar_t* datetime; /// build datetime
        const wchar_t* target;   /// build target platform
        const wchar_t* compiler; /// build this compiler
        const wchar_t* options;  /// build options
        const wchar_t* flags;    /// build with flags
    } build;
    const char* const pid; /// plugin id for internal ident (required)
    // developer info
    const wchar_t* const name;        /// plugin name for user
    const wchar_t* const developer;   /// developer name
    const wchar_t* const description; /// plugin description and more info
    const wchar_t* const copyright;   /// plugin license
    const wchar_t* const email;       /// support email
    const wchar_t* const website;     /// official website
} ucm_plugin_info_t;

/*! Structure what defines base plugin interface*/
typedef struct _ucm_plugin_s
{
    ucm_object_t oid; /// ucm system class object identificator

    ucm_plugin_info_t info; /// plugins description
    ucm_uuid_t uuid;        /// plugins unique ID (UUID)

    UCM_RET(*run)
    (void);                /// activate plugin (with context for hot-plug) (required)
    UCM_RET (*stop)(void); /// deactivate plugin (required)
    void (*message)(uint32_t id, uintptr_t ctx, uint32_t x1,
                    uint32_t x2); /// recieve system messages callback
    // TODO define this prototype
    void (*msg_process)(void);
} ucm_plugin_t;
#define U_PLUGIN(X) ((ucm_plugin_t*)(X))

#define _EVENT_SENDER(obj) ((ucm_plugin_t*)(((ucm_ev_t*)obj)->sender))
