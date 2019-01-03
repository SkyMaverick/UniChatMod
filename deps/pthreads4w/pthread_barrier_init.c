/*
 * pthread_barrier_init.c
 *
 * Description:
 * This translation unit implements barrier primitives.
 *
 * --------------------------------------------------------------------------
 *
 *      Pthreads4w - POSIX Threads for Windows
 *      Copyright 1998 John E. Bossom
 *      Copyright 1999-2018, Pthreads4w contributors
 *
 *      Homepage: https://sourceforge.net/projects/pthreads4w/
 *
 *      The current list of contributors is contained
 *      in the file CONTRIBUTORS included with the source
 *      code distribution. The list can also be seen at the
 *      following World Wide Web location:
 *
 *      https://sourceforge.net/p/pthreads4w/wiki/Contributors/
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "pthread.h"
#include "implement.h"


int
pthread_barrier_init (pthread_barrier_t * barrier,
		      const pthread_barrierattr_t * attr, unsigned int count)
{
  pthread_barrier_t b;

  if (barrier == NULL || count == 0)
    {
      return EINVAL;
    }

  if (NULL != (b = (pthread_barrier_t) calloc (1, sizeof (*b))))
    {
      b->pshared = (attr != NULL && *attr != NULL
		    ? (*attr)->pshared : PTHREAD_PROCESS_PRIVATE);

      b->nCurrentBarrierHeight = b->nInitialBarrierHeight = count;
      b->lock = 0;

      if (0 == sem_init (&(b->semBarrierBreeched), b->pshared, 0))
	    {
	      *barrier = b;
	      return 0;
	    }
      (void) free (b);
    }

  return ENOMEM;
}
