FILES:=services.el services-pkg.el services-systemd.el services-sysvinit.el
VERSION:=0.0.2
PACKAGE_NAME:=services-$(VERSION)

all: $(PACKAGE_NAME).tar

$(PACKAGE_NAME).tar: clean $(FILES)
	mkdir $(PACKAGE_NAME)
	cp $(FILES) $(PACKAGE_NAME)
	tar -cf $(PACKAGE_NAME).tar $(PACKAGE_NAME)

clean:
	rm -f $(PACKAGE_NAME).tar
	rm -rf $(PACKAGE_NAME)

