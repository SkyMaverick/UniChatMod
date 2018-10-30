#ifndef _UCM_DATABASE_H_
#define _UCM_DATABASE_H_

UCM_RET
db_open ( char*      aPath,
          uint32_t   flags );

UCM_RET
db_close (void);

#endif
