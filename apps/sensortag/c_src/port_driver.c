#include <stdio.h>
#include "erl_driver.h"
#include "sensortag.h"

typedef struct {
	ErlDrvPort port;
} example_data;

static ErlDrvData sensortag_drv_start(ErlDrvPort port, char *buff) {
	example_data *d = (example_data *)driver_alloc(sizeof(example_data));
	d->port = port;
	return (ErlDrvData)d;
}

static void sensortag_drv_stop(ErlDrvData handle) {
	driver_free((char *)handle);
}

static void sensortag_drv_output(ErlDrvData handle, char *buffer, int bufflen) {
	example_data *d = (example_data *)handle;
	char fn = buffer[0]; 
    char arg = buffer[1]; 
    char res;

	if(fn == 1) {
		res = scan_for_device();
    }

	driver_output(d->port, &res, 1);
}

ErlDrvEntry sensortag_driver_entry = {
	NULL,
	sensortag_drv_start,
	sensortag_drv_stop,
	sensortag_drv_output,
	NULL,
	NULL,
	"sensortag_drv",
	NULL,
	NULL,
	NULL,
	NULL,
    NULL,
    NULL,   
    NULL,
  NULL,
  NULL,
  ERL_DRV_EXTENDED_MARKER,
  ERL_DRV_EXTENDED_MAJOR_VERSION,
  ERL_DRV_EXTENDED_MINOR_VERSION,
  0,
  NULL,
  NULL,
  NULL
};

DRIVER_INIT(sensortag_drv)
{
	return &sensortag_driver_entry;
}

