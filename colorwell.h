#ifndef __COLORWELL_H__
#define __COLORWELL_H__

#include <gtk/gtk.h>

#define COLOR_WELL(obj)              GTK_CHECK_CAST(obj, color_well_get_type(), ColorWell)
#define COLOR_WELL_CLASS(klass)      GTK_CHECK_CLASS_CAST(klass, color_well_get_type(), ColorWellClass)
#define IS_COLOR_WELL(obj)           GTK_CHECK_TYPE(obj, color_well_get_type())

typedef struct _ColorWell ColorWell;
typedef struct _ColorWellClass ColorWellClass;

struct _ColorWell
{
    GtkVBox vbox;

    GtkPreview *preview;
    GtkColorSelectionDialog *color_selection_dialog;
    gdouble color[4];
};

struct _ColorWellClass
{
    GtkVBoxClass parent_class;

    void (*color_changed) (ColorWell *color_well);
};

guint color_well_get_type (void);
GtkWidget* color_well_new (void);

void color_well_set_color (ColorWell *color_well, gdouble *color);
void color_well_get_color (ColorWell *color_well, gdouble *color);

#endif
