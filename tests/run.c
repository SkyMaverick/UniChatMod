#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <dlfcn.h>
#include <unistd.h>
#include <limits.h>
#include <CUnit/Basic.h>
#include "util.h"

#define PATH_MAX 4096

typedef struct modules_list {
    char* module_path;
    struct modules_list* next;
} module_t;

void (*runSuite)(void);

size_t lookup_suites (char* lookup_dir, module_t** found_suites) {
    size_t suites_count = 0;
    DIR *suites_path = opendir(lookup_dir);
    struct dirent *ls = NULL;
    
    if(!suites_path) {
        fprintf(stderr,"%s error: %s\n",lookup_dir, strerror(errno));
        return -1;
    }   
    
    module_t* tmp_module = NULL;
    while((ls = readdir(suites_path))) {
        if((strncmp(ls->d_name,".",1) == 0) || (strstr(ls->d_name,".so")==0))
            continue;
        
        char buf_path[PATH_MAX];
        snprintf(buf_path,PATH_MAX,"%s/%s",lookup_dir,ls->d_name);

        void* h_module = dlopen(buf_path, RTLD_LAZY);
        char* error = NULL;
        error = dlerror();
        if(!h_module) {
            fprintf(stderr, "Couldn't load module: %s\n", error);
            continue;
        }

        runSuite = dlsym(h_module, "runSuite");
        error = dlerror();
        if (error) {
            fprintf(stderr, "Couldn't load module: %s\n", error);
            dlclose(h_module);
            continue;
        }
        
        module_t* new_module = (module_t*)malloc(sizeof(module_t));
        memset(new_module,0,sizeof(module_t));
        if(tmp_module) {
            tmp_module->next = new_module;
            tmp_module = new_module;
        } else {
            tmp_module = new_module;
            *found_suites = new_module;
        }
        
        fprintf(stdout, "Found test suite: %s\n", ls->d_name);
        new_module->module_path=strdup(buf_path);
    
        suites_count++;
    }

    return suites_count;
}

int main (int argc, char* argv[]) {
    (void) argc;

    char app_path[PATH_MAX];
    char suites_path[PATH_MAX];

    if(!realpath(argv[0], app_path)){
        snprintf(app_path, PATH_MAX,"%s",argv[0]);
    }
    char *e = strrchr(app_path,'/');
    if(e) *e=0;

    snprintf(suites_path, PATH_MAX, "%s/%s", app_path, "suites");
  
    struct stat d_info;
    module_t* suites_list = NULL;

    if(stat(suites_path, &d_info)<0) {
        if(!S_ISDIR(d_info.st_mode)) {
            fprintf(stderr, "%s is not directory\n", suites_path);
            return 1;
        }
    }

    if(access(suites_path, R_OK | X_OK)) {
        fprintf(stderr, "%s is not accessible\n", suites_path);
        return 1;
    }

    size_t suites_count = lookup_suites(suites_path, &suites_list);
    if(suites_count){
        fprintf(stdout, "Found %zu test suites\n", suites_count);
    }else{
        fprintf(stdout, "Suites modules not found\n");
        return 0;
    }

    CUnitInitialize();
    
    module_t* suite = suites_list;
    while(suite) {
        void* h_suite = dlopen(suite->module_path, RTLD_LAZY);
        if(!h_suite) {
            fprintf(stderr, "Module '%s'\n", strerror(errno));
            continue;
        }
        runSuite = dlsym(h_suite, "runSuite");
        runSuite();
        dlclose(h_suite);
        suite=suite->next;
    }

    CU_basic_set_mode(CU_BRM_VERBOSE);
    CU_basic_run_tests();
    CUnitUInitialize();

    // Cleanup
    while (suites_list !=NULL){
        module_t* tmp_suites = suites_list;
        if(suites_list->next){
            suites_list=suites_list->next;}
        else
            suites_list=NULL;
        fprintf(stdout, "Free module: %s\n",tmp_suites->module_path);
        free(tmp_suites->module_path);
        free(tmp_suites);
    }
    return CU_get_error();
}