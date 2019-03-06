

function bindPlot(element: HTMLElement, data: Prop<jquery.flot.dataSeries[]>, options: jquery.flot.plotOptions): void {
    const redraw = () => {
        if ($(element).is(":visible"))
            $.plot($(element), data.get(), options);
    };
    window.setTimeout(redraw, 1);
    window.onresize = redraw;
    data.event(redraw);
}
