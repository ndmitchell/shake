

function bindPlot(element: HTMLElement, data: Prop<jquery.flot.dataSeries[]>, options: jquery.flot.plotOptions): void {
    const redraw = () => {
        if ($(element).is(":visible"))
            $.plot($(element), data.get(), options);
    };
    window.setTimeout(redraw, 1);
    $(window).on("resize", redraw);
    data.event(redraw);
}


function varLink(name: string): HTMLElement {
    return <a href={"https://hackage.haskell.org/package/shake/docs/Development-Shake.html#v:" + name}><tt>{name}</tt></a>;
}
