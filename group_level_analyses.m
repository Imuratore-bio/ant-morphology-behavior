%% corrs, pca, ci

dim = length(vars);
out = {'corr', 'pval', 'both', 'pca', 'eig', 'rotated', ...
    'partial', 'pval', 'both', 'ci graph'};

for crt = 1 : length(grp)
    dat = log(grp{crt});
    % pearson correlations
    [r0, p0] = corrcoef(dat);
    % principal components
    [pc, pe] =  pcacov(r0);
    % rotated components
    rc = rotatefactors(pc(:, 1 : 2) .* sqrt(pe(1 : 2)'));
    % partial correlations
    [rp, pp] = partialcorr(dat);
    % cond dependence graph
    g = graph(rp .* (pp < 0.05) .* (rp ~= 1), vars);
    % save variables
    out(crt + 1, :) = {r0, p0, triu(r0) + tril(p0), pc, pe', ...
        rc, rp, pp, triu(rp) + tril(pp), g};
end

%% corr descriptives

% counts of significantly positive or negative ...
for i = 1 : 5
    % ... Pearson correlations
    disp([sum(out{i + 1, 1} > 0 & out{i + 1, 2} < 0.05, 'all') / 2, ...
        sum(out{i + 1, 1} < 0 & out{i + 1, 2} < 0.05, 'all') / 2])
    % ... partial correlations
    disp([sum(out{i + 1, 7} > 0 & out{i + 1, 8} < 0.05, 'all') / 2 - 11, ...
        sum(out{i + 1, 7} < 0 & out{i + 1, 8} < 0.05, 'all') / 2])
end

%% ci clusters

% analysis of conditional independence graphs
for i = 1 : 5
    g = out{i + 1, 10};
    % remove negative correlations
    g = rmedge(g, find(g.Edges.Weight < 0));
    % compute connected components
    b(:, i) = conncomp(g);
end

%% corr distrib tests

for i = 1 : 5
    % Fisher transform of unique Pearson correlations
    r(:, i) = atanh(squareform(out{i + 1, 1} - eye(size(out{i + 1, 1}))));
    % Fisher transform of unique partial correlations
    p(:, i) = atanh(squareform(out{i + 1, 7} - eye(size(out{i + 1, 1}))));
end

% test for equal variances
vartestn(r, 'testtype', 'leveneabsolute');
vartestn(p, 'testtype', 'leveneabsolute');

% manual levene test
[~, t, s] = anova1(abs(r - mean(r)));
[c, m] = multcompare(s);

[~, t, s] = anova1(abs(r - mean(r)));
[c, m] = multcompare(s);

% simple 1-way anova
[~, t, s] = anova1(r);
[c, m] = multcompare(s);

[~, t, s] = anova1(p);
[c, m] = multcompare(s);

%% pairwise corr diffs

% Fisher transform
for i = 1 : 5
    r(:, :, i) = atanh(out{i + 1, 1});
    p(:, :, i) = atanh(out{i + 1, 7});
end

% degrees of freedom
df = cellfun(@length, grp)' - [2, 22];
% variances after Fisher transform
cv = 1 ./ (df - 1);

% pairwise differences
dr = r - permute(r, [1 2 4 3]);
dp = p - permute(p, [1 2 4 3]);

% corresponding z-scores
zr = dr ./ permute(sqrt(cv(:, 1) + cv(:, 1)'), [3 4 1 2]);
zp = dp ./ permute(sqrt(cv(:, 2) + cv(:, 2)'), [3 4 1 2]);

% corresponding p-values
pr = 2 * normcdf(-abs(zr));
pp = 2 * normcdf(-abs(zp));

% extract unique pairwise differences
[j, k] = find(tril(ones(5), -1));
out2 = {'corr', 'pval', 'both', 'partial', 'pval', 'both'};
for i = 1 : length(j)
    out2{i + 1, 1} = out{j(i) + 1, 1} - out{k(i) + 1, 1};
    out2{i + 1, 2} = squeeze(pr(:, :, j(i), k(i)));
    out2{i + 1, 3} = triu(out2{i + 1, 1}) + tril(out2{i + 1, 2});
    out2{i + 1, 4} = out{j(i) + 1, 7} - out{k(i) + 1, 7};
    out2{i + 1, 5} = squeeze(pp(:, :, j(i), k(i)));
    out2{i + 1, 6} = triu(out2{i + 1, 4}) + tril(out2{i + 1, 5});
end

%% corr diffs stats

% differences in sign (positive <-> negative flips)
dr1 = squeeze(sum(pr < 0.05 & (sign(r) ~= ...
    sign(permute(r, [1 2 4 3]))), [3 4])) / 2;
dp1 = squeeze(sum(pp < 0.05 & (sign(p) ~= ...
    sign(permute(p, [1 2 4 3]))), [3 4])) / 2;

% vs. differences in amplitude (including to n.s.)
dp2 = squeeze(sum(pp < 0.05 & (sign(p) == ...
    sign(permute(p, [1 2 4 3]))), [3 4])) / 2;
dr2 = squeeze(sum(pr < 0.05 & (sign(r) == ...
    sign(permute(r, [1 2 4 3]))), [3 4])) / 2;

disp([sum(dr1(:)), sum(dr2(:))])
disp([sum(dp1(:)), sum(dp2(:))])

% average absolute differences
disp(max(squeeze(mean(abs(dr), [3 4])), [], 'all'))
disp(max(squeeze(mean(abs(dp), [3 4])), [], 'all'))