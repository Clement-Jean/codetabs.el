document.addEventListener('DOMContentLoaded', function() {
    const preElements = document.querySelectorAll('.tab-content pre');
    const DARK_MODE_CLASS = 'dark-mode';
    const THEME_KEY = 'codeTheme';
    
    preElements.forEach(pre => {
        const toggle = document.createElement('span');
        toggle.classList.add('theme-toggle-icon');
        pre.appendChild(toggle);

        toggle.addEventListener('click', (event) => {
            const isDarkMode = pre.classList.contains(DARK_MODE_CLASS);
	    preElements.forEach(p => p.classList.toggle(DARK_MODE_CLASS));
            localStorage.setItem(THEME_KEY, isDarkMode ? 'light' : 'dark');
        });
    });

    const savedTheme = localStorage.getItem(THEME_KEY);
    if (savedTheme === 'dark') {
        preElements.forEach(pre => pre.classList.add(DARK_MODE_CLASS));
    }

    const codeTabs = document.querySelectorAll('.code-tabs');
    codeTabs.forEach(tab => {
	const tabButtons = tab.querySelectorAll('.tab-button');
	const tabContents = tab.querySelectorAll('.tab-content');

	console.log(tab);
	tabButtons.forEach(button => {
	    button.addEventListener('click', () => {
		tabButtons.forEach(btn => btn.classList.remove('active'));
		tabContents.forEach(content => content.classList.remove('active'));

		button.classList.add('active');
		const targetTab = button.getAttribute('data-lang');
		const activeContent = tab.querySelector(`#${targetTab}`);
		if (activeContent) {
		    activeContent.classList.add('active');
		}
	    });
	});

	if (tabButtons.length > 0 && tabContents.length > 0) {
	    tabButtons[0].classList.add('active');
	    tabContents[0].classList.add('active');
	}
    });
});
