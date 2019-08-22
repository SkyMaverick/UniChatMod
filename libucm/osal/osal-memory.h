void* osal_malloc(size_t size);

void* osal_zmalloc(size_t size);

void* osal_calloc(size_t nmem, size_t size);

void* osal_realloc(void* mem, size_t size);

void osal_free(void* mem);

void osal_zmemory(void* mem, size_t size);

int osal_realloc2(void** mem, size_t size);

char* osal_strdup(const char* str);

char* osal_strndup(const char* str, size_t num);

#define osal_free_null(X) \
    do {                  \
        osal_free(X);     \
        X = NULL;         \
    } while (0)
