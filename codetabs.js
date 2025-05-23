// see: https://stackoverflow.com/a/62189798/11269045
function uniqueId(length = 16) {
    return parseInt(Math.ceil(Math.random() * Date.now()).toPrecision(length).toString().replace(".", ""))
}

// see: https://www.reddit.com/r/emacs/comments/1k7dic1/comment/mpbuv3l
function blockRunFrom(init) {
    let next = init; let run = [];
    while (next) {
	if (!next.matches('.org-src-container') || !next.hasAttribute('lang')) break;
	run.push(next);
	next = next.nextElementSibling;
    }
    return run;
}

function addTabsFor(content, srcBlocks) {
    if (srcBlocks.length < 1) {
	return null;
    }

    const codetab = document.createElement('div');
    const controls = document.createElement('div');

    codetab.className = 'code-tabs';
    controls.className = 'tab-controls';

    for (const block of srcBlocks) {
	const targetId = 'codetab-'+uniqueId();
	const tab = document.createElement('button');
	tab.className = 'tab-button';
	block.setAttribute('target-id', targetId);
	tab.setAttribute('target-id', targetId);
	if (block.hasAttribute('name')) {
	    tab.innerHTML = block.getAttribute('name');
	} else {
	    tab.innerHTML = block.getAttribute('lang');
	}

	if (block.hasAttribute('emphasize-class')) {
	    const starts = block.getAttribute('emphasize-start').split(',');
	    const ends = block.getAttribute('emphasize-end').split(',');
	    const classes = block.getAttribute('emphasize-class').split(',');
	    const pre = block.querySelector('pre');

	    for (var i = 0; i < classes.length; i++) {
		pre.innerHTML = pre.innerHTML.replace(starts[i], `<span class=${classes[i]}>`)
		pre.innerHTML = pre.innerHTML.replace(ends[i], '</span>')
	    }
	}

	controls.appendChild(tab);
	block.classList.add('tab-content');
    }

    const parent = srcBlocks[0].parentElement;
    parent.insertBefore(codetab, srcBlocks[0]);
    codetab.appendChild(controls);
    srcBlocks.map((block) => codetab.appendChild(block));
    return codetab;
}

document.addEventListener('DOMContentLoaded', () => {
    const content = document.querySelector('#content');
    const selector = (c =>`${c}[lang]:not(${c}[lang] + ${c}[lang])`)('.org-src-container'); // :has(+ ${c}[lang])
    const potentialGroups = Array.from(document.querySelectorAll(selector));
    var codetabs = [];

    potentialGroups.map((group) => {
	const srcBlocks = blockRunFrom(group);
	const codetab = addTabsFor(content, srcBlocks);

	if (codetab != null) {
	    codetabs.push(codetab);
	}
    });

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

    codetabs.forEach(tab => {
	const tabButtons = tab.querySelectorAll('.tab-button');
	const tabContents = tab.querySelectorAll('.tab-content');

	tabButtons.forEach(button => {
	    button.addEventListener('click', () => {
		tabButtons.forEach(btn => btn.classList.remove('active'));
		tabContents.forEach(content => content.classList.remove('active'));

		button.classList.add('active');
		const targetTab = button.getAttribute('target-id');
		const activeContent = tab.querySelector(`div[target-id|='${targetTab}']`);
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
