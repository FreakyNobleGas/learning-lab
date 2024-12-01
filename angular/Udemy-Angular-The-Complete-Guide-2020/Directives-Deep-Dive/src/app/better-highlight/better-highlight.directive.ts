import { Directive, ElementRef, OnInit, Renderer2, HostListener, HostBinding, Input } from "@angular/core";

@Directive({
    selector: '[appBetterHighlight]'
})
export class BetterHighlightDirective implements OnInit {
    @Input() defaultColor: string = 'transparent';
    @Input() highlightColor: string = 'blue';
    // Binds to the host element
    @HostBinding('style.backgroundColor') backgroundColor : string = this.defaultColor;
    
    constructor(private elRef: ElementRef, private renderer: Renderer2) {}

    ngOnInit() {
        // render takes a html element, a style, and the style attribute. There are many more methods in renderer other than setStyle.
        // this.renderer.setStyle(this.elRef.nativeElement, 'background-color', 'blue');

        this.backgroundColor = this.defaultColor;
    }

    // mouseenter and mouseleave are built-in functions for HostListener
    @HostListener('mouseenter') mouseover(data : Event) {
        //this.renderer.setStyle(this.elRef.nativeElement, 'background-color', 'blue');
        this.backgroundColor = this.highlightColor;
    }

    @HostListener('mouseleave') mouseleave(data : Event) {
        //this.renderer.setStyle(this.elRef.nativeElement, 'background-color', 'transparent');
        this.backgroundColor = this.defaultColor;
    }
}